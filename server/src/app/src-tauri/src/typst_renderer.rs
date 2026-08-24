use std::collections::HashMap;
use std::sync::OnceLock;

use typst::diag::{FileError, FileResult};
use typst::foundations::{Bytes, Datetime, Duration};
use typst::syntax::{FileId, RootedPath, Source, VirtualPath, VirtualRoot};
use typst::text::{Font, FontBook};
use typst::utils::LazyHash;
use typst::{Library, LibraryExt, World};
use typst_kit::fonts::FontStore;

struct FontEnv {
    library: LazyHash<Library>,
    book: LazyHash<FontBook>,
    fonts: FontStore,
}

fn font_env() -> &'static FontEnv {
    static ENV: OnceLock<FontEnv> = OnceLock::new();
    ENV.get_or_init(|| {
        let mut fonts = FontStore::new();
        fonts.extend(typst_kit::fonts::embedded());
        let book = LazyHash::new(fonts.book().clone().into_inner());
        FontEnv {
            library: LazyHash::new(Library::default()),
            book,
            fonts,
        }
    })
}

struct SimpleWorld {
    env: &'static FontEnv,
    source: Source,
    attachments: HashMap<FileId, Bytes>,
}

impl SimpleWorld {
    fn new(text: &str, attachments: &[(String, Vec<u8>)]) -> Self {
        let attachments = attachments
            .iter()
            .filter_map(|(path, bytes)| {
                let vpath = VirtualPath::new(path).ok()?;
                let id = RootedPath::new(VirtualRoot::Project, vpath).intern();
                Some((id, Bytes::new(bytes.clone())))
            })
            .collect();
        Self {
            env: font_env(),
            source: Source::detached(text),
            attachments,
        }
    }
}

impl World for SimpleWorld {
    fn library(&self) -> &LazyHash<Library> {
        &self.env.library
    }

    fn book(&self) -> &LazyHash<FontBook> {
        &self.env.book
    }

    fn main(&self) -> FileId {
        self.source.id()
    }

    fn source(&self, id: FileId) -> FileResult<Source> {
        if id == self.source.id() {
            Ok(self.source.clone())
        } else {
            Err(FileError::NotFound(id.vpath().get_without_slash().into()))
        }
    }

    fn file(&self, id: FileId) -> FileResult<Bytes> {
        self.attachments
            .get(&id)
            .cloned()
            .ok_or_else(|| FileError::NotFound(id.vpath().get_without_slash().into()))
    }

    fn font(&self, index: usize) -> Option<Font> {
        self.env.fonts.font(index)
    }

    fn today(&self, _offset: Option<Duration>) -> Option<Datetime> {
        None
    }
}

pub const DEFAULT_WIDTH_PT: f64 = 340.157480315;
const MIN_WIDTH_PT: f64 = 100.0;
const MAX_WIDTH_PT: f64 = 900.0;

fn sanitize_ink(ink: &str) -> &str {
    let hex = ink.strip_prefix('#').unwrap_or(ink);
    let valid = (hex.len() == 3 || hex.len() == 6) && hex.bytes().all(|b| b.is_ascii_hexdigit());
    if valid {
        ink
    } else {
        "#000000"
    }
}

fn card_preamble(width_pt: f64, ink: &str) -> String {
    let width_pt = if width_pt.is_finite() {
        width_pt.clamp(MIN_WIDTH_PT, MAX_WIDTH_PT)
    } else {
        DEFAULT_WIDTH_PT
    };
    let ink = sanitize_ink(ink);
    format!(
        "#set page(width: {width_pt}pt, height: auto, margin: (rest: 0.1cm, bottom: 0.4cm), fill: none)\n#set text(size: 14pt, fill: rgb(\"{ink}\"))\n"
    )
}

pub struct RenderOptions<'a> {
    pub width_pt: f64,
    pub preamble: &'a str,
    pub attachments: &'a [(String, Vec<u8>)],
    pub ink: &'a str,
}

impl Default for RenderOptions<'_> {
    fn default() -> Self {
        Self {
            width_pt: DEFAULT_WIDTH_PT,
            preamble: "",
            attachments: &[],
            ink: "black",
        }
    }
}

pub fn render_as_svg(text: &str, options: RenderOptions) -> Result<String, String> {
    let source = format!(
        "{}{}{text}",
        card_preamble(options.width_pt, options.ink),
        options.preamble
    );
    let world = SimpleWorld::new(&source, options.attachments);

    let warned = typst::compile::<typst_layout::PagedDocument>(&world);
    let document = warned.output.map_err(|errors| {
        errors
            .iter()
            .map(|e| e.message.to_string())
            .collect::<Vec<_>>()
            .join("; ")
    })?;

    let page = document
        .pages()
        .first()
        .ok_or_else(|| "document has no pages".to_string())?;
    Ok(typst_svg::svg(page, &typst_svg::SvgOptions::default()))
}
