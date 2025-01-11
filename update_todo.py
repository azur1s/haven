import re
import os
from glob import glob
import pathspec

def load_gitignore_rules(gitignore_path=".gitignore"):
    if os.path.exists(gitignore_path):
        with open(gitignore_path, "r", encoding="utf-8") as gitignore_file:
            gitignore_rules = gitignore_file.read().splitlines()
        return pathspec.PathSpec.from_lines("gitwildmatch", gitignore_rules)
    return pathspec.PathSpec.from_lines("gitwildmatch", [])

def extract_todo_comments_from_file(file_path):
    todo_comments = []
    comment_pattern = re.compile(r"\(\*(.*?)\*\)", re.DOTALL)

    try:
        with open(file_path, 'r', encoding='utf-8') as file:
            content = file.read()
            comments = comment_pattern.findall(content)
            todo_comments = [comment.strip() for comment in comments if "TODO" in comment]
    except (UnicodeDecodeError, IOError):
        pass

    return todo_comments

def scan_src_folder_for_todos(src_folder, gitignore_spec):
    todos_by_file = {}
    # Search for all .ml and .ich files in the src folder (recursively)
    ocaml_files = glob(os.path.join(src_folder, "**/*.ml"), recursive=True)
    ocaml_files += glob(os.path.join(src_folder, "**/*.ich"), recursive=True)

    for file_path in ocaml_files:
        # Normalize paths for matching with .gitignore rules
        relative_path = os.path.relpath(file_path, start=src_folder)
        if gitignore_spec.match_file(relative_path):
            continue

        todos = extract_todo_comments_from_file(file_path)
        if todos:
            todos_by_file[file_path] = todos

    return todos_by_file

def update_readme_with_todos(readme_path, todos_by_file):
    if not os.path.exists(readme_path):
        print(f"README.md not found: {readme_path}")
        return

    new_todo_section = "## TODO\n\n"
    if todos_by_file:
        for file_path, todos in todos_by_file.items():
            escaped_file_path = file_path.replace("\\", "/")  # Normalize file paths for readability
            new_todo_section += f"### {escaped_file_path}\n"
            for todo in todos:
                new_todo_section += f"- {todo}\n"
    else:
        new_todo_section += "No TODO comments found.\n"

    # Read the current README.md content
    with open(readme_path, 'r', encoding='utf-8') as readme_file:
        readme_content = readme_file.read()

    # Replace the `## TODO` section
    updated_readme_content = re.sub(
        r"## TODO.*?(?=\n##|$)", new_todo_section.strip(), readme_content, flags=re.DOTALL
    )

    # Check if there are any changes
    if updated_readme_content != readme_content:

        # Write changes to the file
        with open(readme_path, 'w', encoding='utf-8') as readme_file:
            readme_file.write(updated_readme_content)
        print("\nREADME.md updated successfully.")
    else:
        print("No changes needed. README.md is already up-to-date.")

if __name__ == "__main__":
    src_folder = "./"  # Scan current folder and subdirectories
    readme_path = "README.md"  # Assumes README.md is in the current directory
    gitignore_spec = load_gitignore_rules(".gitignore")

    if not os.path.exists(src_folder):
        print(f"Folder not found: {src_folder}")
    else:
        todos_by_file = scan_src_folder_for_todos(src_folder, gitignore_spec)
        if todos_by_file:
            update_readme_with_todos(readme_path, todos_by_file)
        else:
            print("No TODO comments found in the folder. Hooray!")
