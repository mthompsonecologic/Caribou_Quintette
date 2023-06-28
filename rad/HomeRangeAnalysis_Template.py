#!/usr/bin/local/python3
import argparse
from glob import glob

parser = argparse.ArgumentParser(description = "Generates R and abash scripts from template file", formatter_class=argparse.RawDescriptionHelpFormatter)
parser.add_argument("--Templates", "-T", help = "path to directory with templates", required = True)
parser.add_argument("--SAs", "-A", help = "space separated list of possible SAs i.e. default BM TR", required = False, nargs = "+", default= ["BM", "TR"])
parser.add_argument("--Season", "-S", help = "space separated list of possible seasons i.e. default WI CA RU SU", required = False, nargs = "+", default= ["WI", "CA", "RU", "SU"])
parser.add_argument("--Project", "-P", help = "Client Name", required = False, default= "Caribou Quintette")

args = parser.parse_args()

def writefile(data, outname):
	with open(outname, "w") as writer:
		writer.writelines(data)
	return

def main():
	# Read in template files
	Templates = glob("{}/*".format(args.Templates))
	Templates.sort(reverse = True)
	TemplateNames = [temp.split("/")[-1] for temp in Templates]
	# print(TemplateNames)
	ScriptI = open(Templates[0]).readlines()
	ScriptI = "".join(ScriptI)
	JobI = open(Templates[1]).readlines()
	JobI = "".join(JobI)

	# Get Directory Names
	for sa in args.SAs:
		print(sa)
		for season in args.Season:
			print(season)
			DataDirectories = glob("data/{}/{}/*".format(sa, season))
			# print(DataDirectories)
			# Get Number of Jobs
			numbers = [i.split("_")[-1] for i in DataDirectories]
			# print(numbers)
			for number in numbers:
				outdirectory = "data/{}/{}/Job_{}".format(sa, season, number)
				# print(outdirectory)
				# if number < 10:
				# 	number = "0" + str(number)
				# else:
				# 	number = str(number)
				# number = str(number)
				# Update Script I
				scriptI = ScriptI.replace("**JOBID**", number)
				scriptI = scriptI.replace("**SA**", sa)
				scriptI = scriptI.replace("**SEASON**", season)
				scriptIname = outdirectory + "/" + TemplateNames[0].replace("Template", "{}_{}_Job_{}".format(sa, season, number))
				writefile(scriptI, scriptIname)

				# Update Job I
				jobI = JobI.replace("**JOBID**", number)
				jobI = jobI.replace("**SA**", sa)
				jobI = jobI.replace("**PROJECT**", args.Project)
				jobI = jobI.replace("**SEASON**", season)
				jobIname =  outdirectory + "/" + TemplateNames[1].replace("Template", "{}_{}_Job_{}".format(sa, season, number))
				writefile(jobI, jobIname)


	return



if __name__ == '__main__':
	main()