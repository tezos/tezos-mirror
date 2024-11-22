# Building the Hermit Loader

## Checkout the right repository

Check out [hermit-os/loader][hermit-loader-repo] at the following commit.

    commit 6eb4677db78d371b8818bdd6afee84f240ec0dfa (HEAD, tag: v0.4.6)
    Merge: 4af71ad f4bf2f1
    Author: Martin Kröning <mkroening@posteo.net>
    Date:   Tue Feb 20 09:52:41 2024 +0000

        Merge pull request #304 from hermit-os/release-0.4.6
        
        chore: release version 0.4.6

Using any later version causes problems because the entry ABI for the Hermit kernel changes.

[hermit-loader-repo]: https://github.com/hermit-os/loader

## Patch the Loader such that it starts at address 0

We want our loader to start at address 0. Otherwise the loader will waste all memory before it. E.g.
if you put the loader at address 2GiB, those two gebibytes are unusable.

Apply the following patch to fix this.

    diff --git a/src/arch/riscv64/link.ld b/src/arch/riscv64/link.ld
    index 2210e67..2597753 100644
    --- a/src/arch/riscv64/link.ld
    +++ b/src/arch/riscv64/link.ld
    @@ -1,7 +1,7 @@
     SECTIONS {
       kernel_start = ADDR (.text.start);
       
    -  .text.start 0x80200000 : { *(.text._start) }
    +  .text.start 0x0 : { *(.text._start) }
       .text : { *(.text.*) }
       .rodata : { *(.rodata.*) }
       .data : { *(.data.*) }

## Compile the Loader

You can compile the loader with the following command.

    cargo run -p xtask -- build --target riscv64 --release

Once complete, copy the `target/riscv64/release/hermit-loader` from the Loader repository into the 
directory of this markdown file. 
