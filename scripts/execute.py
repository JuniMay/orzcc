#! /usr/bin/env python3

import argparse
import difflib
import os
import shutil
import subprocess
from typing import Any, Dict
import re
import datetime
import csv


def check_file(file1, file2, diff_file):
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
        diff = difflib.unified_diff(
            list(map(lambda x: x.strip(), f1.readlines())),
            list(map(lambda x: x.strip(), f2.readlines())),
            fromfile=file1,
            tofile=file2,
        )
        diff_list = list(diff)

        if diff_file is not None:
            with open(diff_file, 'w') as f:
                f.writelines(diff_list)

        if len(diff_list) > 0:
            # compare float number with 1e-6 precision
            f1.seek(0)
            f2.seek(0)
            str1 = re.split(r'[ \n]', f1.read())
            str2 = re.split(r'[ \n]', f2.read())
            str1 = list(filter(lambda x: x != '', str1))
            str2 = list(filter(lambda x: x != '', str2))
            if (len(str1) != len(str2)):
                return False
            for i in range(len(str1)):
                if (str1[i] != str2[i]):
                    try:
                        float1 = float.fromhex(str1[i])
                        float2 = float.fromhex(str2[i])
                        if (abs(float1 - float2) > 1e-6):
                            return False
                    except:
                        return False

        return True


def execute(command, timeout) -> Dict[str, Any]:
    try:
        result = subprocess.run(command,
                                shell=True,
                                timeout=timeout,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,
                                universal_newlines=True)

        return {
            'returncode': result.returncode,
            'stdout': result.stdout,
            'stderr': result.stderr,
        }

    except subprocess.TimeoutExpired:
        return {
            'returncode': None,
            'stdout': '',
            'stderr': 'TIMEOUT',
        }
    except Exception as e:
        return {
            'returncode': None,
            'stdout': '',
            'stderr': str(e),
        }


def execute_testcase(executable, input_file, output_file, timeout,
                     std_output_file):
    command = (f'{executable} <{input_file} >{output_file}'
               if input_file is not None else f'{executable} >{output_file}')
    try:
        result = subprocess.run(command,
                                shell=True,
                                timeout=timeout,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE,
                                universal_newlines=True)

        # Get time from stderr using regex
        # Add result into us (microseconds)
        # Format: TOTAL: xH-xM-xS-xus
        # e.g. TOTAL: 0H-0M-3S-940755us
        time = re.search(r'TOTAL: (\d+)H-(\d+)M-(\d+)S-(\d+)us', result.stderr)
        if time:
            time = (int(time.group(1)) * 3600 * 1000000 +
                    int(time.group(2)) * 60 * 1000000 +
                    int(time.group(3)) * 1000000 + int(time.group(4)))
        else:
            time = -1

        need_newline = False
        with open(output_file, 'r') as f:
            content = f.read()
            if len(content) > 0:
                if not content.endswith('\n'):
                    need_newline = True

        # add return code to the last line of out file
        with open(output_file, 'a+') as f:
            if need_newline:
                f.write('\n')
            f.write(str(result.returncode))
            f.write('\n')

        is_equal = check_file(output_file, std_output_file, None)

        if is_equal:
            return ('AC', time)
        else:
            return ('WA', -1)

    except subprocess.TimeoutExpired:
        return ('TLE', -1)

    except Exception as e:
        print(e)
        return ('RE', -1)


def test_native(executable_path: str,
                testcase_dir: str,
                output_dir: str,
                runtime_lib_dir: str,
                exec_timeout: int,
                opt_level: int,
                csv_path: str,
                is_performance: bool = False):

    testcase_list = []

    def dfs(curr_dir: str):
        dir_list = sorted(os.listdir(curr_dir))

        for file_or_dir in dir_list:
            full_path = os.path.join(curr_dir, file_or_dir)

            if os.path.isfile(full_path) and full_path.endswith('.sy'):
                testcase_list.append(full_path.rsplit('.', 1)[0])

            elif os.path.isdir(full_path):
                dfs(full_path)

    dfs(testcase_dir)

    result = []

    # create csv file if not exist
    if not os.path.isfile(csv_path):
        with open(csv_path, 'w+') as f:
            csv_writer = csv.DictWriter(f, fieldnames=['time'])
            csv_writer.writeheader()

    with open(csv_path, 'r') as f:
        csv_reader = csv.DictReader(f)
        for row in csv_reader:
            result.append(row)

    result.append({})

    test_time = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    result[-1]['time'] = test_time

    for testcase in testcase_list:

        basename: str = os.path.basename(testcase)

        in_path = f'{testcase}.in'

        std_out_path = f'{testcase}.out'
        if not os.path.isfile(in_path):
            in_path = None

        asm_path = os.path.join(output_dir, f'{basename}.s')
        exec_path = os.path.join(output_dir, f'{basename}')

        command = f'{executable_path} {testcase}.sy -S -o {asm_path} -O{opt_level}'

        execute(command, exec_timeout)

        command = f'gcc -march=rv64gc {asm_path} -L{runtime_lib_dir} -lsylib -o {exec_path}'

        execute(command, exec_timeout)

        status, time = execute_testcase(exec_path, in_path,
                                        f'{output_dir}/{basename}.out',
                                        exec_timeout, std_out_path)

        print(f'{testcase}: {status} {time}')

        result[-1][basename] = f'{time}({status})'

    if is_performance:
        with open(csv_path, 'w+') as f:
            csv_writer = csv.DictWriter(f, fieldnames=result[0].keys())
            csv_writer.writeheader()
            csv_writer.writerows(result)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--timeout', type=int, default=600)
    parser.add_argument('--opt-level', type=int, default=0)
    parser.add_argument('--output-dir', default='./output')
    parser.add_argument('--testcase-dir', default='./tests/sysy')
    parser.add_argument('--runtime-lib-dir', default='./sysy-runtime-lib')
    parser.add_argument('--csv-file', default='./result.csv')

    parser.add_argument('--executable-path', default='./target/release/orzcc')

    parser.add_argument('--no-compile', action='store_true', default=False)
    parser.add_argument('--no-test', action='store_true', default=False)

    parser.add_argument('--native', action='store_true', default=False)

    parser.add_argument('--performance', action='store_true', default=False)

    return parser.parse_args()


def compile(timeout: int):
    command = (f'cargo build --release')

    print(f'BUILDING')
    result = execute(command, timeout=timeout)

    print(result['stdout'])

    if result['returncode'] != 0:
        print('BUILDING FAILED.\n')
        print(result['stderr'])
        exit(1)
    else:
        print('BUILDING FINISHED.\n')


def log(logfile, command, exec_result):
    logfile.write(f'EXECUTE: {command}\n')
    logfile.write(f'STDOUT:\n')
    logfile.write(exec_result['stdout'])
    logfile.write(f'STDERR:\n')
    logfile.write(exec_result['stderr'])
    logfile.write(f'\n')


def test(executable_path: str, testcase_dir: str, output_dir: str,
         runtime_lib_dir: str, exec_timeout: int, opt_level: int):

    testcase_list = []

    def dfs(curr_dir: str):
        dir_list = sorted(os.listdir(curr_dir))

        for file_or_dir in dir_list:
            full_path = os.path.join(curr_dir, file_or_dir)

            if os.path.isfile(full_path) and full_path.endswith('.sy'):
                testcase_list.append(full_path.rsplit('.', 1)[0])

            elif os.path.isdir(full_path):
                dfs(full_path)

    dfs(testcase_dir)

    result_md = f"# Test Result\n\n"
    testcase_cnt = len(testcase_list)
    correct_cnt = 0
    result_md_table = f"| Testcase | Status |\n"
    result_md_table += f"| -------- | ------ |\n"

    for testcase in testcase_list:
        basename: str = os.path.basename(testcase)

        in_path = f'{testcase}.in'

        std_out_path = f'{testcase}.out'
        if not os.path.isfile(in_path):
            in_path = None

        ast_path = os.path.join(output_dir, f'{basename}.ast')
        ir_path = os.path.join(output_dir, f'{basename}.orzir')
        asm_path = os.path.join(output_dir, f'{basename}.s')
        out_path = os.path.join(output_dir, f'{basename}.out')
        exec_path = os.path.join(output_dir, f'{basename}')
        diff_path = os.path.join(output_dir, f'{basename}.diff')

        log_path = os.path.join(output_dir, f'{basename}.log')
        log_file = open(log_path, 'w')

        command = (f'{executable_path} compile '
                   f'--file {testcase}.sy '
                   f'--emit-ast {ast_path} '
                   f'--emit-ir {ir_path} '
                   f'--emit-asm {asm_path} ')

        exec_result = execute(command, exec_timeout)
        log(log_file, command, exec_result)

        if exec_result['returncode'] is None:
            if exec_result['stderr'] == 'TIMEOUT':
                result_md_table += f"| `{testcase}` | ⚠️ orzcc TLE |\n"
                print(f'[  ERROR  ] (orzcc TLE) {testcase}')
            else:
                result_md_table += f"| `{testcase}` | ⚠️ orzcc RE |\n"
                print(f'[  ERROR  ] (orzcc RE) {testcase}')

            continue

        command = (f'riscv64-linux-gnu-gcc -march=rv64gc {asm_path}'
                    f' -L{runtime_lib_dir} -lsylib -o {exec_path}')

        exec_result = execute(command, exec_timeout)
        log(log_file, command, exec_result)

        if exec_result['returncode'] is None or exec_result['stderr'] != '':
            result_md_table += f"| `{testcase}` | 😢 CE |\n"
            print(f'[  ERROR  ] (CE) {testcase}, see: ', log_path)
            continue

        command = (f'qemu-riscv64 -L /usr/riscv64-linux-gnu {exec_path}'
                   f' >{out_path}') if in_path is None else (
                       f'qemu-riscv64 -L /usr/riscv64-linux-gnu {exec_path}'
                       f' <{in_path} >{out_path}')

        exec_result = execute(command, exec_timeout)

        need_newline = False
        with open(out_path, 'r') as f:
            content = f.read()
            if len(content) > 0:
                if not content.endswith('\n'):
                    need_newline = True

        # add return code to the last line of out file
        with open(out_path, 'a+') as f:
            if need_newline:
                f.write('\n')
            f.write(str(exec_result['returncode']))
            f.write('\n')

        is_equal = check_file(out_path, std_out_path, diff_path)

        if exec_result['returncode'] is None:
            if exec_result['stderr'] == 'TIMEOUT':
                result_md_table += f'| `{testcase}` | ⏱️ TLE |\n'
                print(f'[  ERROR  ] (TLE) {testcase}, check: {asm_path}')
            else:
                # SOS icon
                result_md_table += f'| `{testcase}` | 🆘 RE |\n'
                print(f'[  ERROR  ] (RE) {testcase}, see: {log_path}')
        elif is_equal:
            correct_cnt += 1
            result_md_table += f'| `{testcase}` | ✅ AC |\n'
            print(f'[ CORRECT ] (AC) {testcase}')
        else:
            result_md_table += f'| `{testcase}` | ❌ WA |\n'
            print(f'[  ERROR  ] (WA) {testcase}, see: {log_path}')

        log(log_file, command, exec_result)

    result_md += f'Passed {correct_cnt}/{testcase_cnt} testcases.\n\n'

    result_md += result_md_table

    with open(f'{output_dir}/result.md', 'w') as f:
        f.write(result_md)


def main():
    args = parse_args()

    if not args.no_compile:
        compile(args.timeout)

    if not args.no_test:
        if os.path.exists(args.output_dir):
            shutil.rmtree(args.output_dir)

        if not os.path.exists(args.output_dir):
            os.makedirs(args.output_dir)

        if args.native:
            # wait for cooling
            import time
            time.sleep(5)

            test_native(args.executable_path, args.testcase_dir,
                        args.output_dir, args.runtime_lib_dir, args.timeout,
                        args.opt_level, args.csv_file, args.performance)
        else:
            test(args.executable_path,
                 args.testcase_dir,
                 args.output_dir,
                 args.runtime_lib_dir,
                 args.timeout,
                 args.opt_level)


if __name__ == '__main__':
    main()