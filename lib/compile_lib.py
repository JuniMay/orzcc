import os

# riscv64-linux-gnu-gcc ./lib/orzcc-runtime-lib.c -S -o ./lib/orzcc-runtime-lib.s -O3 -DNDEBUG -march=rv64gc_zba_zbb -fno-stack-protector -fomit-frame-pointer -mcpu=sifive-u74 -mabi=lp64d -mcmodel=medlow -ffp-contract=on -fno-ident -z noexecstack
def compile_runtime_lib():
    os.system("riscv64-linux-gnu-gcc ./lib/orzcc-runtime-lib.c -S -o ./lib/orzcc-runtime-lib.s -O3 -DNDEBUG -march=rv64gc_zba_zbb -fno-stack-protector -fomit-frame-pointer -mcpu=sifive-u74 -mabi=lp64d -mcmodel=medlow -ffp-contract=on -fno-ident -z noexecstack")
    # remove the first two lines
    with open("./lib/orzcc-runtime-lib.s", "r") as f:
        lines = f.readlines()
    with open("./lib/orzcc-runtime-lib.s", "w") as f:
        f.writelines(lines[2:])

if __name__ == "__main__":
    compile_runtime_lib()