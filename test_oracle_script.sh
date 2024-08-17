#!/bin/bash

# 检查是否提供了参数
if [ -z "$1" ]; then
  echo "Usage: $0 <opt-level>"
  echo "opt-level: 0 or 1"
  exit 1
fi

# 获取优化级别参数
opt_level=$1

# 检查参数是否为0或1
if [ "$opt_level" != "0" ] && [ "$opt_level" != "1" ]; then
  echo "Invalid opt-level: $opt_level"
  echo "opt-level must be 0 or 1"
  exit 1
fi

# 包含.sy文件的目录
sy_directory="./tests/sysy/external_test"

# 要执行的基本命令
command_base="python3 ./scripts/test_oracle.py"

echo "Running test_oracle for external tests (opt level $opt_level)"
echo ""
# 遍历目录中的.sy文件
for file in "$sy_directory"/*.sy; do
    # 使用当前文件作为参数执行命令
    $command_base "$file" --timeout 600 --opt-level $opt_level --output-dir ./output --executable-path ./target/release/compiler --no-compile
done