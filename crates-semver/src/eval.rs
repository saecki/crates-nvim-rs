// copy pasta from the semver crate

use crate::{Comparator, Op, Version, VersionReq};

pub(crate) fn matches_requirement(req: &VersionReq, ver: &Version) -> bool {
    for cmp in &req.comparators {
        if !matches_impl(cmp, ver) {
            return false;
        }
    }

    if ver.pre.is_empty() {
        return true;
    }

    // If a version has a prerelease tag (for example, 1.2.3-alpha.3) then it
    // will only be allowed to satisfy req if at least one comparator with the
    // same major.minor.patch also has a prerelease tag.
    for cmp in &req.comparators {
        if pre_is_compatible(cmp, ver) {
            return true;
        }
    }

    false
}

pub(crate) fn matches_comparator(cmp: &Comparator, ver: &Version) -> bool {
    matches_impl(cmp, ver) && (ver.pre.is_empty() || pre_is_compatible(cmp, ver))
}

fn matches_impl(cmp: &Comparator, ver: &Version) -> bool {
    match cmp.op {
        Op::Eq | Op::Wl => matches_exact(cmp, ver),
        Op::Gt => matches_greater(cmp, ver),
        Op::Ge => matches_exact(cmp, ver) || matches_greater(cmp, ver),
        Op::Lt => matches_less(cmp, ver),
        Op::Le => matches_exact(cmp, ver) || matches_less(cmp, ver),
        Op::Tl => matches_tilde(cmp, ver),
        Op::Cr | Op::Bl => matches_caret(cmp, ver),
    }
}

fn matches_exact(cmp: &Comparator, ver: &Version) -> bool {
    if let Some(major) = cmp.version.major() {
        if ver.major != major {
            return false;
        }
    }

    if let Some(minor) = cmp.version.minor() {
        if ver.minor != minor {
            return false;
        }
    }

    if let Some(patch) = cmp.version.patch() {
        if ver.patch != patch {
            return false;
        }
    }

    &ver.pre == cmp.version.pre()
}

fn matches_greater(cmp: &Comparator, ver: &Version) -> bool {
    match cmp.version.major() {
        None => unreachable!(">* is not a valid requirement"),
        Some(major) => {
            if ver.major != major {
                return ver.major > major;
            }
        }
    }

    match cmp.version.minor() {
        None => return false,
        Some(minor) => {
            if ver.minor != minor {
                return ver.minor > minor;
            }
        }
    }

    match cmp.version.patch() {
        None => return false,
        Some(patch) => {
            if ver.patch != patch {
                return ver.patch > patch;
            }
        }
    }

    &ver.pre > cmp.version.pre()
}

fn matches_less(cmp: &Comparator, ver: &Version) -> bool {
    match cmp.version.major() {
        None => unreachable!("<* is not a valid requirement"),
        Some(major) => {
            if ver.major != major {
                return ver.major < major;
            }
        }
    }

    match cmp.version.minor() {
        None => return false,
        Some(minor) => {
            if ver.minor != minor {
                return ver.minor < minor;
            }
        }
    }

    match cmp.version.patch() {
        None => return false,
        Some(patch) => {
            if ver.patch != patch {
                return ver.patch < patch;
            }
        }
    }

    &ver.pre < cmp.version.pre()
}

fn matches_tilde(cmp: &Comparator, ver: &Version) -> bool {
    if Some(ver.major) != cmp.version.major() {
        return false;
    }

    if let Some(minor) = cmp.version.minor() {
        if ver.minor != minor {
            return false;
        }
    }

    if let Some(patch) = cmp.version.patch() {
        if ver.patch != patch {
            return ver.patch > patch;
        }
    }

    &ver.pre >= cmp.version.pre()
}

fn matches_caret(cmp: &Comparator, ver: &Version) -> bool {
    let Some(major) = cmp.version.major() else {
        unreachable!("^* is not a valid requirement")
    };

    if ver.major != major {
        return false;
    }

    let Some(minor) = cmp.version.minor() else {
        return true;
    };

    let Some(patch) = cmp.version.patch() else {
        return if major > 0 {
            ver.minor >= minor
        } else {
            ver.minor == minor
        };
    };

    if major > 0 {
        if ver.minor != minor {
            return ver.minor > minor;
        } else if ver.patch != patch {
            return ver.patch > patch;
        }
    } else if minor > 0 {
        if ver.minor != minor {
            return false;
        } else if ver.patch != patch {
            return ver.patch > patch;
        }
    } else if ver.minor != minor || ver.patch != patch {
        return false;
    }

    &ver.pre >= cmp.version.pre()
}

fn pre_is_compatible(cmp: &Comparator, ver: &Version) -> bool {
    cmp.version.major() == Some(ver.major)
        && cmp.version.minor() == Some(ver.minor)
        && cmp.version.patch() == Some(ver.patch)
        && !cmp.version.pre().is_empty()
}
