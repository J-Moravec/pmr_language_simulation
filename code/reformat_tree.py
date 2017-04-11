import dendropy
import argparse

def parse_args():
    parser = argparse.ArgumentParser(
        prog = "reformat_tree",
        description = (
            "This script will just read and write again tree/multiple trees."
            " This helps against some hiccups like misformed translate block"
            " (as in trees from Fiona Jordan)."
            )
        )
    parser.add_argument(
        "nexus", help="Nexus file with single or multiple trees."
        )
    parser.add_argument(
        "output", help="Path for reformated trees."
        )
    parser.add_argument(
        "-m", "--multiple", default=True, action="store_true",
        help="If nexus file contains single or multiple trees"
        )
    args = parser.parse_args()
    return(args)


def main(args):
    if(args.multiple):
        tree = dendropy.TreeList.get(path=args.nexus, schema="nexus", preserve_underscores=True)
    else:
        tree = dendropy.Tree.get(path=args.nexus, schema="nexus", preserve_underscores=True)
    tree.write(
        path=args.output,
        schema="nexus", unquoted_underscores=True, suppress_annotations=True,
        translate_tree_taxa=True        
        )


if __name__ == "__main__":
    args = parse_args()
    main(args)
