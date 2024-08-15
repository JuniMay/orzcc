import os

def transport_files(input_dir, output_dir):
    # Get a list of all files with the .stdout extension in the input directory
    input_files = [f for f in os.listdir(input_dir) if f.endswith('.stdout')]

    for input_file in input_files:
        # Construct the corresponding output file path
        output_file = os.path.join(output_dir, input_file.replace('.stdout', '.out'))

        # Read the contents of the input file
        with open(os.path.join(input_dir, input_file), 'r') as f:
            content = f.read()

        # Write the contents to the output file
        with open(output_file, 'w') as f:
            f.write(content)

# transport stdout(clang) -> tests/sysy/external_test/*.out
input_dir = '/Users/fengsicheng/Desktop/orzcc/output'
output_dir = '/Users/fengsicheng/Desktop/orzcc/tests/sysy/external_test'
transport_files(input_dir, output_dir)