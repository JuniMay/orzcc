import difflib

def compare_orzir_files(file1, file2):
    with open(file1, 'r') as f1, open(file2, 'r') as f2:
        lines1 = f1.readlines()
        lines2 = f2.readlines()

    diff = difflib.unified_diff(lines1, lines2)

    for line in diff:
        print(line)

# Example usage
file1 = '/path/to/file1.orzir'
file2 = '/path/to/file2.orzir'
compare_orzir_files(file1, file2)