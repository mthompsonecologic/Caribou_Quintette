#!/usr/bin/local/python3
import pandas as pd
import argparse
import os

parser = argparse.ArgumentParser(description = "SPlits the caribou data to be passed into Hoe Ranage Analysis Templates", formatter_class=argparse.RawDescriptionHelpFormatter)
parser.add_argument("--RawData", "-R", help = "path to raw data to be split", required = False)
args = parser.parse_args()


def main():
    # raw = pd.read_csv("data/Caribou_noclip.csv",parse_dates=["date"])
    # animalid = set(raw["AnimalID"]) # COLUMN 0
    # print(animalid)
    # print(len(animalid))
    # season = set(raw["season"]) COLUMN 3
    # print(season)
    # print(len(season))
    # areatag = set(raw["areatag"]) COLUMN 5
    # print(areatag)
    # print(len(areatag))
    # sa = set(raw["SA"]) COLUMN 8
    # print(len(sa))
    # yearseas = set(raw["YearSeas"]) COLUMN 9
    # print(yearseas)
    # print(len(yearseas))
    # Create Diretories for every possible season and areatag combination
	raw = open("data/Caribou_noclip.csv").readlines()
	header = raw.pop(0)
	header = header.replace(",", "\t")
	unique_directories = set()
	parsed_data = dict()
	# winter = ["EW", "MW", "LW"]
	# calving = "CA"
	# summer = "SU"
	# rutting = "RU"
	for line in raw:
		parsed_line = line.strip().split(",")
		sa = parsed_line[8].replace('"', "")
		season = parsed_line[3].replace('"', "")
		if season in ["EW", "MW", "LW"]:
			season = "WI"
			# print(season)
		key = sa + "_" + season
		animalid = parsed_line[0].replace('"', "")
		uid = key + "_" + animalid
		data = (parsed_line[1].replace('"', ""), parsed_line[2].replace('"', ""), parsed_line[4].replace('"', ""), parsed_line[5].replace('"', ""), parsed_line[6].replace('"', ""), parsed_line[7].replace('"', ""), parsed_line[9].replace('"', ""), parsed_line[10].replace('"', ""))
		if parsed_data.__contains__(key):
			if parsed_data[key].__contains__(animalid):
				parsed_data[key][animalid].append(data)
			else:
				parsed_data[key][animalid] = list()
				parsed_data[key][animalid].append(data)
		else:
			parsed_data[key] = dict()
			parsed_data[key][animalid] = list()
			parsed_data[key][animalid].append(data)
		unique_directories.add(uid)
	print(unique_directories)
	#Generate Directory Tree
	for directory in unique_directories:
		parsed_directory =  directory.split("_")
		parent = "data/" +parsed_directory[0]
		sub_parent = parent + "/" + parsed_directory[1]
		child = sub_parent + "/" + parsed_directory[2]
		try:
			os.mkdir(parent)
		except:
			print("Top level exist: {}".format(parent))
		try:
			os.mkdir(sub_parent)
		except:
			print("Top level exist: {}".format(sub_parent))
		# try:
		# 	os.mkdir(child)
		# except:
		# 	print("Top level exist: {}".format(child))
	print(len(unique_directories))
	# print(len(parsed_data))
	for keys, items in parsed_data.items():
		par_dir = "data/" + keys.replace("_", "/")
		# print("\n\n")
		# print(keys)
		# print(items)
		# print(len(items))
		total = len(items)
		keys2 = list(items.keys())
		# print("Total: {}".format(total))
		# if total == len(keys2):
		# 	print("total checks out")
		# SPlit into 10 animal jobs
		whole = int(total / 10)
		print("Whole: {}".format(whole))
		remainder = total % 10
		print("Remainder: {}".format(remainder))
		print("Indices")
		jobcounter = 0
		for idx in range(10, whole*10+20, 10):
			low_dir = "{}/Job_{}".format(par_dir, jobcounter)
			sa = par_dir.split("/")[0]
			season = par_dir.split("/")[1]
			keys2_2 = keys2[idx-10:idx]
			if len(keys2_2) == 0:
				continue
			# Make lowest level directory and csv
			try:
				os.mkdir(low_dir)
			except:
				print("lowest level directory exist: {}".format(low_dir))
			with open("{}/Job_{}.txt".format(low_dir, jobcounter), "w") as writer:
				writer.writelines(header)
				for subanimal in keys2_2:
					data = items[subanimal]
					for datum in data:
						# print(datum)
						line = "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\n".format(
						subanimal,
						datum[0],
						datum[1],
						season,
						datum[2],
						datum[3],
						datum[4],
						datum[5],
						sa,
						datum[6], 
						datum[7]
						)
						writer.writelines(line)
			jobcounter += 1	
		# for keys2, items2 in items.items():
		# 	print(keys2)
		# 	print(len(items2))
			

	

          

	return

if __name__ == '__main__':
	main()