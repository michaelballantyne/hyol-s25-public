import argparse

class AppendTransformed(argparse.Action):
    def __init__(self, *args, **kwargs):
        self.transform = kwargs.pop("transform")
        super().__init__(*args, **kwargs)
    def __call__(self, parser, namespace, values, option_string=None):
        dest = getattr(namespace, self.dest) or []
        dest.append(self.transform(values))
        setattr(namespace, self.dest, dest)

def parse_args():
    parser = argparse.ArgumentParser(description="A grep-like tool with filtering options.")

    parser.add_argument("pattern", help="The search pattern (regex or string).")
    parser.add_argument("path", help="The path to search in.")

    parser.add_argument(
        "--include", 
        action=AppendTransformed,
        transform=lambda p: ("include", p),
        dest="filters", 
        metavar="pattern",
        help="pattern to include"
    )

    parser.add_argument(
        "--exclude", 
        action=AppendTransformed,
        transform=lambda p: ("exclude", p),
        dest="filters", 
        metavar="pattern",
        help="pattern to exclude"
    )

    parser.add_argument(
        "-c", 
        action="store_true", 
        help="Only print a count of matching lines per file."
    )

    parser.add_argument(
        "-r", 
        action="store_true", 
        help="Recursively search directories."
    )

    return parser.parse_args()

if __name__ == "__main__":
    args = parse_args()
    print(args)