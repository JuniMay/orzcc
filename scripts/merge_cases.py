import os
import shutil

# merge all performance cases (include the final performance cases) into one directory
# source_dirs = [
#     './tests/testcases/2023/performance',
#     './tests/testcases/2022/performance',
#     './tests/testcases/2021/performance_test2021-private',
#     './tests/testcases/2021/performance_test2021-public',
#     './tests/testcases/2021/2021初赛所有用例/performance',
#     './tests/testcases/2020/section1/performance_test',
#     './tests/testcases/2020/section2/performance_test',
#     './tests/testcases/2023/final_performance',
#     './tests/testcases/2022/final_performance',
# ]
# output_dir = './tests/testcases/union/performance'

# merge all functional cases into one directory
source_dirs = [
    './tests/testcases/2023/functional',
    './tests/testcases/2023/hidden_functional',
    './tests/testcases/2022/functional',
    './tests/testcases/2022/hidden_functional',
    './tests/testcases/2021/function_test2020',
    './tests/testcases/2021/function_test2021',
    './tests/testcases/2021/2021初赛所有用例/functional',
    './tests/testcases/2021/2021初赛所有用例/h_functional',
    './tests/testcases/2020/section1/functional_test',
]

output_dir = './tests/testcases/union/function'


# 创建输出目录如果它不存在
os.makedirs(output_dir, exist_ok=True)

# 创建一个集合以存储已经处理过的文件名，防止重复复制
handled_files = set()

# 定义一个函数来复制目录中的文件
def copy_files(source_dir):
    for file_name in os.listdir(source_dir):
        # 检查文件是否已处理
        if file_name not in handled_files:
            # 文件未处理，添加到集合并复制到目标目录
            handled_files.add(file_name)
            source_file = os.path.join(source_dir, file_name)
            target_file = os.path.join(output_dir, file_name)
            # 复制文件
            shutil.copy(source_file, target_file)

# 循环处理每个源目录
for dir_path in source_dirs:
    copy_files(dir_path)

print("所有文件已成功复制到", output_dir)
