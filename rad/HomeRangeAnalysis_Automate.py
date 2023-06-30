from glob import glob
import argparse
import os

parser = argparse.ArgumentParser(description = "Generates R and abash scripts from template file", formatter_class=argparse.RawDescriptionHelpFormatter)
parser.add_argument("--Data", "-D", help = "path to data folder", required = True)

args = parser.parse_args()


def main():
	bashes = glob("{}/**/*sh".format(args.Data), recursive = True)
	bashes = [os.path.abspath(path) for path in bashes]
	template = "cd **path**\nsbatch **bashfile**cd **datapath**\n\n"
	paths = [i.rsplit("/", 1)[0] for i in bashes]
	bashfiles = [i.rsplit("/", 1)[1] for i in bashes]
	# print(paths)
	# print("/n/n")
	# print(bashfiles)
	for idx in range(len(bashes)):
		temp = template.replace("**path**", paths[idx])
	return

if __name__ == '__main__':
	main()