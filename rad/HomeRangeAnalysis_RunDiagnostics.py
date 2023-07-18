#!/usr/bin/env python3
from glob import glob
import argparse
import os

parser = argparse.ArgumentParser(description = "Generates R and abash scripts from template file", formatter_class=argparse.RawDescriptionHelpFormatter)
parser.add_argument("--Command", "-D", help = "path to data folder", required = True)

args = parser.parse_args()


def main():
	bashes = glob("{}/**/*sh".format(args.Data), recursive = True)
	bashes = [os.path.abspath(path) for path in bashes]
	template = "cd **path**\nsbatch **bashfile**\ncd **datapath**\n\n\n"
	paths = [i.rsplit("/", 1)[0] for i in bashes]
	bashfiles = [i.rsplit("/", 1)[1] for i in bashes]
	# print(paths)
	# print("/n/n")
	# print(bashfiles)
	with open("HomeRangeAnalysis_Automate_Submission.sh", "w") as writer:
		for idx in range(len(bashes)):
			temp = template.replace("**path**", paths[idx])
			temp = temp.replace("**bashfile**", bashfiles[idx])
			temp = temp.replace("**datapath**", os.path.abspath(args.Data))
			writer.writelines(temp)

	return

if __name__ == '__main__':
	main()