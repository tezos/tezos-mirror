//! Module for parsing and installing packages

use anyhow::Context;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use url::Url;
use wasmer_registry::WasmerConfig;

/// Source of a package
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum PackageSource {
    /// Download from a URL
    Url(Url),
    /// Run a local file
    File(String),
    /// Download from a package
    Package(wasmer_registry::Package),
}

impl Default for PackageSource {
    fn default() -> Self {
        PackageSource::File(String::new())
    }
}

impl FromStr for PackageSource {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}

impl PackageSource {
    /// Parses a package source and transforms it to a URL or a File
    pub fn parse(s: &str) -> Result<Self, String> {
        // If the file is a http:// URL, run the URL
        if let Ok(url) = url::Url::parse(s) {
            if url.scheme() == "http" || url.scheme() == "https" {
                return Ok(Self::Url(url));
            }
        }

        Ok(match wasmer_registry::Package::from_str(s) {
            Ok(o) => Self::Package(o),
            Err(_) => Self::File(s.to_string()),
        })
    }

    /// Downloads the package (if any) to the installation directory, returns the path
    /// of the package directory (containing the wasmer.toml)
    pub fn download_and_get_filepath(&self) -> Result<PathBuf, anyhow::Error> {
        let url = match self {
            Self::File(f) => {
                let path = Path::new(&f).to_path_buf();
                return if path.exists() {
                    Ok(path)
                } else {
                    Err(anyhow::anyhow!("Could not find local file {f}"))
                };
            }
            Self::Url(u) => {
                let wasmer_dir = WasmerConfig::get_wasmer_dir()
                    .map_err(|e| anyhow::anyhow!("no wasmer dir: {e}"))?;
                if let Some(path) =
                    wasmer_registry::Package::is_url_already_installed(u, &wasmer_dir)
                {
                    return Ok(path);
                } else {
                    u.clone()
                }
            }
            Self::Package(p) => {
                let wasmer_dir = WasmerConfig::get_wasmer_dir()
                    .map_err(|e| anyhow::anyhow!("no wasmer dir: {e}"))?;
                let package_path = Path::new(&p.file()).to_path_buf();
                if package_path.exists() {
                    return Ok(package_path);
                } else if let Some(path) = p.already_installed(&wasmer_dir) {
                    return Ok(path);
                } else {
                    let config = WasmerConfig::from_file(&wasmer_dir)
                        .map_err(|e| anyhow::anyhow!("error loading wasmer config file: {e}"))?;
                    p.url(&config.registry.get_current_registry())?
                }
            }
        };

        let extra = if let Self::Package(p) = self {
            format!(", local file {} does not exist either", p.file())
        } else {
            String::new()
        };

        let wasmer_dir =
            WasmerConfig::get_wasmer_dir().map_err(|e| anyhow::anyhow!("no wasmer dir: {e}"))?;
        let mut sp = start_spinner(format!("Installing package {url} ..."));
        let opt_path = wasmer_registry::install_package(&wasmer_dir, &url);
        if let Some(sp) = sp.take() {
            use std::io::Write;
            sp.clear();
            let _ = std::io::stdout().flush();
        }

        let path = opt_path
            .with_context(|| anyhow::anyhow!("Could not fetch package from URL {url}{extra}"))?;

        Ok(path)
    }
}

fn start_spinner(msg: String) -> Option<spinoff::Spinner> {
    if !isatty::stdout_isatty() {
        return None;
    }
    #[cfg(target_os = "windows")]
    {
        use colored::control;
        let _ = control::set_virtual_terminal(true);
    }
    Some(spinoff::Spinner::new(
        spinoff::Spinners::Dots,
        msg,
        spinoff::Color::White,
    ))
}

#[test]
fn test_package_source() {
    assert_eq!(
        PackageSource::parse("registry.wapm.io/graphql/python/python").unwrap(),
        PackageSource::File("registry.wapm.io/graphql/python/python".to_string()),
    );

    assert_eq!(
        PackageSource::parse("/absolute/path/test.wasm").unwrap(),
        PackageSource::File("/absolute/path/test.wasm".to_string()),
    );

    assert_eq!(
        PackageSource::parse("C://absolute/path/test.wasm").unwrap(),
        PackageSource::File("C://absolute/path/test.wasm".to_string()),
    );

    assert_eq!(
        PackageSource::parse("namespace/name@latest").unwrap(),
        PackageSource::Package(wasmer_registry::Package {
            namespace: "namespace".to_string(),
            name: "name".to_string(),
            version: Some("latest".to_string()),
        })
    );

    assert_eq!(
        PackageSource::parse("namespace/name@latest:command").unwrap(),
        PackageSource::File("namespace/name@latest:command".to_string()),
    );

    assert_eq!(
        PackageSource::parse("namespace/name@1.0.2").unwrap(),
        PackageSource::Package(wasmer_registry::Package {
            namespace: "namespace".to_string(),
            name: "name".to_string(),
            version: Some("1.0.2".to_string()),
        })
    );

    assert_eq!(
        PackageSource::parse("namespace/name@1.0.2-rc.2").unwrap(),
        PackageSource::Package(wasmer_registry::Package {
            namespace: "namespace".to_string(),
            name: "name".to_string(),
            version: Some("1.0.2-rc.2".to_string()),
        })
    );

    assert_eq!(
        PackageSource::parse("namespace/name").unwrap(),
        PackageSource::Package(wasmer_registry::Package {
            namespace: "namespace".to_string(),
            name: "name".to_string(),
            version: None,
        })
    );

    assert_eq!(
        PackageSource::parse("https://wapm.io/syrusakbary/python").unwrap(),
        PackageSource::Url(url::Url::parse("https://wapm.io/syrusakbary/python").unwrap()),
    );

    assert_eq!(
        PackageSource::parse("command").unwrap(),
        PackageSource::File("command".to_string()),
    );

    assert_eq!(
        PackageSource::parse("python@latest").unwrap(),
        PackageSource::File("python@latest".to_string()),
    );
}
