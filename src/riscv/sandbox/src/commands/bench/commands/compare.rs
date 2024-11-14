use crate::{cli::BenchCompareOptions, commands::bench::load_from_file, table};
use itertools::Itertools;
use std::error::Error;

pub fn compare(opts: BenchCompareOptions) -> Result<(), Box<dyn Error>> {
    let files = opts.comparison_files()?;

    let data = files
        .into_iter()
        .map(|f| load_from_file(&f).map(|s| (s, f)))
        .collect::<Result<Vec<_>, _>>()?;

    let d = data.iter().map(|(d, f)| (d, f)).collect_vec();
    let t = table::table_from_stats(&opts.sort_args, d.as_slice());
    println!("{t}");
    Ok(())
}
