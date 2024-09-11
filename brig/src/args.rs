use clap::{Parser, ValueEnum, ValueHint};

#[derive(Parser)]
pub struct Args {
    /// Target architecture. Defaults to the host architecture.
    #[arg(short, long)]
    pub target: Option<Target>,

    /// The output file. If not specified, output will be printed to stdout.
    #[arg(short, long)]
    pub output: Option<String>,

    /// The input files
    ///
    /// Currently only supports a single input file.
    #[arg(value_hint = ValueHint::FilePath)]
    pub input: Vec<String>,
}

#[derive(Clone, Debug, ValueEnum)]
pub enum Target {
    LinuxX86,
}
