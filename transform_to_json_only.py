#!/usr/bin/env python3
"""
Transform all commands from dual output mode (JSON + human) to JSON-only mode.

Pattern to apply to each command:
1. Remove: let is_json = flag.get_bool(input.flags, "json") |> result.unwrap(False)
2. Remove: let mode = output_mode.from_json_flag(is_json)
3. For each `case is_json { True -> ... False -> ... }`:
   - Extract the True branch content
   - Remove 2 spaces of indentation
   - Replace the entire case block with the extracted content
4. Remove: |> glint.flag("json", flag.bool() |> flag.default(False) |> flag.description("Output as JSON"))
"""

import re
import sys

def remove_is_json_declaration(content):
    """Remove let is_json = flag.get_bool(input.flags, "json") |> result.unwrap(False)"""
    pattern = r'    let is_json =\n      flag\.get_bool\(input\.flags, "json"\)\n      \|> result\.unwrap\(False\)\n\n'
    return re.sub(pattern, '', content)

def remove_mode_declaration(content):
    """Remove let mode = output_mode.from_json_flag(is_json)"""
    pattern = r'    let mode = output_mode\.from_json_flag\(is_json\)\n\n'
    return re.sub(pattern, '', content)

def extract_true_branch(case_block):
    """Extract the True branch content from a case is_json block"""
    lines = case_block.split('\n')
    result = []
    in_true_branch = False
    in_false_branch = False
    brace_depth = 0
    skip_first_true = True

    for line in lines:
        if 'case is_json {' in line:
            continue
        elif line.strip() == 'True ->' or (line.strip().startswith('True ->') and not line.strip().endswith('{')):
            in_true_branch = True
            skip_first_true = True
            # Check if there's content on the same line after True ->
            after_arrow = line.split('True ->', 1)[1].strip()
            if after_arrow and not after_arrow.startswith('case'):
                # Content on same line
                result.append(' ' * (len(line) - len(line.lstrip()) - 2) + after_arrow)
                skip_first_true = False
            continue
        elif line.strip() == 'False ->' or line.strip().startswith('False ->'):
            in_true_branch = False
            in_false_branch = True
            continue
        elif in_true_branch:
            # Remove 2 spaces of indentation
            if skip_first_true and line.strip() == '':
                continue
            skip_first_true = False
            if line.startswith('          '):
                result.append(line[2:])
            elif line.startswith('        '):
                result.append(line[2:])
            else:
                result.append(line)
        elif in_false_branch:
            # Skip false branch content
            continue

    # Remove trailing closing brace from case
    if result and result[-1].strip() == '}':
        result.pop()

    return '\n'.join(result)

def transform_case_is_json_blocks(content):
    """Transform all case is_json blocks to JSON-only"""
    # This is complex because of nested structures
    # We need to match case is_json { ... } blocks carefully

    result = []
    lines = content.split('\n')
    i = 0

    while i < len(lines):
        line = lines[i]

        # Check if this line contains "case is_json {"
        if 'case is_json {' in line:
            # Find the matching closing brace
            block_start = i
            brace_depth = 1
            j = i + 1

            while j < len(lines) and brace_depth > 0:
                if '{' in lines[j]:
                    brace_depth += lines[j].count('{')
                if '}' in lines[j]:
                    brace_depth -= lines[j].count('}')
                j += 1

            # Extract the case block
            case_block = '\n'.join(lines[i:j])

            # Extract True branch
            true_content = extract_true_branch(case_block)

            # Add the transformed content
            if true_content.strip():
                result.append(true_content)

            i = j
        else:
            result.append(line)
            i += 1

    return '\n'.join(result)

def remove_json_flag_definitions(content):
    """Remove |> glint.flag("json", ...) definitions"""
    # Match the multiline flag definition
    pattern = r'  \|> glint\.flag\(\n    "json",\n    flag\.bool\(\) \|> flag\.default\(False\) \|> flag\.description\("Output as JSON"\),\n  \)\n'
    content = re.sub(pattern, '', content)

    # Also match single-line variant
    pattern2 = r'  \|> glint\.flag\(\n    "json",\n    flag\.bool\(\)\n      \|> flag\.default\(False\)\n      \|> flag\.description\("Output as JSON"\),\n  \)\n'
    content = re.sub(pattern2, '', content)

    return content

def main():
    input_file = 'src/intent.gleam'
    output_file = 'src/intent.gleam'

    # Read the file
    with open(input_file, 'r') as f:
        content = f.read()

    # Apply transformations
    print("Removing is_json declarations...")
    content = remove_is_json_declaration(content)

    print("Removing mode declarations...")
    content = remove_mode_declaration(content)

    print("Transforming case is_json blocks...")
    content = transform_case_is_json_blocks(content)

    print("Removing JSON flag definitions...")
    content = remove_json_flag_definitions(content)

    # Write the result
    with open(output_file, 'w') as f:
        f.write(content)

    print("Transformation complete!")

if __name__ == '__main__':
    main()
