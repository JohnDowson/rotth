// pub mod asm;
// pub mod cranelift;
pub mod cranelift2;
// pub mod llvm;

fn disasm() -> capstone::Capstone {
    use capstone::arch::BuildsCapstone;
    use capstone::arch::BuildsCapstoneSyntax;
    let cs = capstone::prelude::Capstone::new();
    #[cfg(target_arch = "aarch64")]
    {
        let mut cs = cs
            .arm64()
            .mode(capstone::prelude::arch::arm64::ArchMode::Arm)
            .detail(true)
            .build()
            .unwrap();
        cs.set_skipdata(true).unwrap();
        cs
    }
    #[cfg(target_arch = "x86_64")]
    {
        cs.x86()
            .mode(capstone::prelude::arch::x86::ArchMode::Mode64)
            .syntax(capstone::prelude::arch::x86::ArchSyntax::Att)
            .detail(true)
            .build()
            .unwrap()
    }
}
