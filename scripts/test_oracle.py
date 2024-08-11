#! /usr/bin/env python3

import argparse
import os
from execute import check_file, execute, compile, log, execute_testcase

def parse_args():
    parser = argparse.ArgumentParser()
    # test_oracle.py <testcase.sy> --timeout 600 --opt-level 0 --output-dir ./output --executable-path ./target/release/compiler --no-compile
    parser.add_argument("testcase", type=str)
    parser.add_argument("--timeout", type=int, default=600)
    parser.add_argument("--opt-level", type=int, default=0)
    parser.add_argument("--output-dir", default="./output")
    parser.add_argument("--executable-path", default="./target/release/compiler")
    parser.add_argument("--no-compile", action="store_true", default=False)
    parser.add_argument("--runtime-lib-dir", default="./tests/sysy/sysy-runtime-lib")

    return parser.parse_args()

def test_one(test_case, timeout, opt_level, output_dir, executable_path, runtime_lib_dir):
    if not os.path.isfile(test_case):
        print(f"\033[33m[  ERROR  ] (no such file)\033[0m {test_case}")
        return False
    
    if not test_case.endswith(".sy"):
        print(f"\033[33m[  ERROR  ] (not a .sy file)\033[0m {test_case}")
        return False

    # use orzcc to compile the target
    in_path = test_case.replace(".sy", ".in")

    if not os.path.isfile(in_path):
        in_path = None

    basename = os.path.basename(test_case).split(".")[0]
    ir_path = os.path.join(output_dir, f"{basename}.orzir")
    asm_path = os.path.join(output_dir, f"{basename}.s")
    out_path = os.path.join(output_dir, f"{basename}.out")
    exec_path = os.path.join(output_dir, f"{basename}")
    std_ir_path = os.path.join(output_dir, f"{basename}.ll")
    std_asm_path = os.path.join(output_dir, f"{basename}.std.s")
    std_exec_path = os.path.join(output_dir, f"{basename}.std")
    std_out_path = os.path.join(output_dir, f"{basename}.stdout")
    diff_path = os.path.join(output_dir, f"{basename}.diff")

    log_path = os.path.join(output_dir, f"{basename}.log")
    log_file = open(log_path, "w")
    
    command = (
        f"clang -w -xc {test_case} "
        f"./tests/sysy/sysy-runtime-lib-fix/sylib.c "
        f"-o {std_exec_path}"
    )

    exec_result = execute(command, timeout)
    log(log_file, command, exec_result)

    if exec_result["returncode"] is None or exec_result["stderr"] != "":
        print(f"\033[33m[  ERROR  ] (clang CE)\033[0m {test_case}, see: ", log_path)
        return False
    
    command = (
        (f"{std_exec_path}" f" >{std_out_path}")
        if in_path is None
        else (
            f"{std_exec_path}"
            f" <{in_path} >{std_out_path}"
        )
    )

    exec_result = execute(command, timeout)
    log(log_file, command, exec_result)
    
    need_newline = False
    with open(std_out_path, "r") as f:
        content = f.read()
        if len(content) > 0:
            if not content.endswith("\n"):
                need_newline = True

    # add return code to the last line of out file
    with open(std_out_path, "a+") as f:
        if need_newline:
            f.write("\n")
        f.write(str(exec_result["returncode"]))
        f.write("\n")

    if exec_result["returncode"] is None:
        if exec_result["stderr"] == "TIMEOUT":
            print(f"\033[33m[  ERROR  ] (clang obj TLE)\033[0m {test_case}, check: {asm_path}")
        else:
            # SOS icon
            print(f"\033[35m[  ERROR  ] (clang obj RE)\033[0m {test_case}, see: {log_path}")
        return False
    
    command = (
            f"{executable_path} -S "
            f"-o {asm_path} "
            f"{test_case} "
            f"--emit-ir {ir_path} "
            f"-O{opt_level}"
        )
    
    exec_result = execute(command, timeout)
    log(log_file, command, exec_result)
    
    if exec_result["returncode"] is None:
        if exec_result["stderr"] == "TIMEOUT":
            print(f"\033[33m[  ERROR  ] (orzcc TLE)\033[0m {test_case}")
        else:
            print(f"\033[35m[  ERROR  ] (orzcc RE)\033[0m {test_case}")
        return False
    
    command = (
            f"riscv64-linux-gnu-gcc -march=rv64gc {asm_path}"
            f" -L{runtime_lib_dir} -lsylib -o {exec_path}"
        )

    exec_result = execute(command, timeout)
    log(log_file, command, exec_result)

    if exec_result["returncode"] is None or exec_result["stderr"] != "":
        print(f"\033[33m[  ERROR  ] (orzcc CE)\033[0m {test_case}, see: ", log_path)
        return False

    command = (
        (f"qemu-riscv64 -L /usr/riscv64-linux-gnu {exec_path}" f" >{out_path}")
        if in_path is None
        else (
            f"qemu-riscv64 -L /usr/riscv64-linux-gnu {exec_path}"
            f" <{in_path} >{out_path}"
        )
    )

    exec_result = execute(command, timeout)
    log(log_file, command, exec_result)

    need_newline = False
    with open(out_path, "r") as f:
        content = f.read()
        if len(content) > 0:
            if not content.endswith("\n"):
                need_newline = True

    # add return code to the last line of out file
    with open(out_path, "a+") as f:
        if need_newline:
            f.write("\n")
        f.write(str(exec_result["returncode"]))
        f.write("\n")

    if exec_result["returncode"] is None:
        if exec_result["stderr"] == "TIMEOUT":
            print(f"\033[33m[  ERROR  ] (orzcc obj TLE)\033[0m {test_case}, check: {asm_path}")
        else:
            print(f"\033[35m[  ERROR  ] (orzcc obj RE)\033[0m {test_case}, see: {log_path}")
        return False

    # get the standard output
    # command = (
    #     f"clang -xc {test_case} "
    #     f"-include {runtime_lib_dir}/sylib.h "
    #     f"-S -o {std_asm_path}"
    # )

    # exec_result = execute(command, timeout)
    # log(log_file, command, exec_result)

    # if exec_result["returncode"] is None:
    #     if exec_result["stderr"] == "TIMEOUT":
    #         print(f"\033[33m[  ERROR  ] (clang -> asm TLE)\033[0m {test_case}")
    #     else:
    #         print(f"\033[35m[  ERROR  ] (clang -> asm RE)\033[0m {test_case}")
    #     return False
    
    # command = (
    #     f"clang --target=riscv64 -mabi=lp64d -S {std_ir_path} "
    #     f"-o {std_asm_path}"
    # )

    # exec_result = execute(command, timeout)
    # log(log_file, command, exec_result)

    # if exec_result["returncode"] is None:
    #     if exec_result["stderr"] == "TIMEOUT":
    #         print(f"\033[33m[  ERROR  ] (clang ir -> asm TLE)\033[0m {test_case}")
    #     else:
    #         print(f"\033[35m[  ERROR  ] (clang ir -> asm RE)\033[0m {test_case}")
    #     return False

    # compare the output
    is_equal = check_file(out_path, std_out_path, diff_path)

    if is_equal:
        print(f"\033[32m[    OK    ]\033[0m {test_case}")
    else:
        print(f"\033[31m[  FAILED  ]\033[0m {test_case}, see: {diff_path}")

    log_file.close()

    return is_equal

def main():
    args = parse_args()
    # compile the orzcc
    if not args.no_compile:
        compile(args.timeout)
    
    if not os.path.exists(args.output_dir):
        os.makedirs(args.output_dir)

    test_one(args.testcase, args.timeout, args.opt_level, args.output_dir, args.executable_path, args.runtime_lib_dir)


if __name__ == "__main__":
    main()
