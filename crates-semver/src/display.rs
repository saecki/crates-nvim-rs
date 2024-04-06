use crate::{CompVersion, Comparator, Op, Version, VersionReq};
use std::fmt::Write;

struct Wrapper<'a, 'b> {
    pos: usize,
    writer: &'b mut std::fmt::Formatter<'a>,
}

impl<'a, 'b> Wrapper<'a, 'b> {
    pub fn new(writer: &'b mut std::fmt::Formatter<'a>) -> Self {
        Self { pos: 0, writer }
    }
}

impl<'a, 'b> std::fmt::Write for Wrapper<'a, 'b> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.pos += s.len();
        self.writer.write_str(s)
    }
}

impl std::fmt::Display for VersionReq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = Wrapper::new(f);
        fmt_requirement(&mut f, self)
    }
}

impl std::fmt::Display for Comparator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = Wrapper::new(f);
        fmt_comparator(&mut f, self)
    }
}

impl std::fmt::Display for CompVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = Wrapper::new(f);
        fmt_comp_version(&mut f, self)
    }
}

fn fmt_requirement(f: &mut Wrapper<'_, '_>, req: &VersionReq) -> std::fmt::Result {
    let [first, others @ ..] = req.comparators.as_slice() else {
        return Ok(());
    };

    fmt_comparator(f, first)?;

    for c in others {
        let num_spaces = c.op_offset.char as usize - f.pos;
        write!(f, "{:num_spaces$}", "")?;
        fmt_comparator(f, c)?;
    }

    Ok(())
}

fn fmt_comparator(f: &mut Wrapper<'_, '_>, comparator: &Comparator) -> std::fmt::Result {
    let Comparator {
        op_offset,
        op,
        version_offset,
        version,
        comma,
    } = comparator;

    f.pos = op_offset.char as usize;

    match op {
        Op::Eq => write!(f, "=")?,
        Op::Lt => write!(f, "<")?,
        Op::Le => write!(f, "<=")?,
        Op::Gt => write!(f, ">")?,
        Op::Ge => write!(f, ">=")?,
        Op::Cr => write!(f, "^")?,
        Op::Tl => write!(f, "~")?,
        Op::Wl => (),
        Op::Bl => (),
    }

    let num_spaces = version_offset.char as usize - f.pos;
    write!(f, "{:num_spaces$}", "")?;
    fmt_comp_version(f, version)?;

    if let Some(comma_offset) = comma {
        let num_spaces = comma_offset.char as usize - f.pos;
        write!(f, "{:num_spaces$},", "")?;
    }

    Ok(())
}

fn fmt_comp_version(f: &mut Wrapper<'_, '_>, version: &CompVersion) -> std::fmt::Result {
    match version {
        CompVersion::Star => write!(f, "*"),
        CompVersion::Major(major) => write!(f, "{major}"),
        CompVersion::MajorWl(major) => write!(f, "{major}.*"),
        CompVersion::Minor(major, minor) => write!(f, "{major}.{minor}"),
        CompVersion::MinorWl(major, minor) => write!(f, "{major}.{minor}.*"),
        CompVersion::Patch(major, minor, patch) => write!(f, "{major}.{minor}.{patch}"),
        CompVersion::Pre(major, minor, patch, pre) => {
            write!(f, "{major}.{minor}.{patch}-{}", pre.as_str())
        }
    }
}

impl std::fmt::Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            major,
            minor,
            patch,
            pre,
            meta,
        } = self;
        write!(f, "{major}.{minor}.{patch}")?;
        if !pre.is_empty() {
            write!(f, "-{}", pre.as_str())?;
        }
        if !meta.is_empty() {
            write!(f, "+{}", meta.as_str())?;
        }
        Ok(())
    }
}
