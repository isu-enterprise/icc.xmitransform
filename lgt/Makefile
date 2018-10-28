.PHONY: all tests st itests gt igt t i transform run irun

all: tests

t: tests

i: itests

tests:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester},halt."

itests:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester},logtalk_load(tools(loader))."

gt:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester_gen},halt."

igt:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester_gen}."

st:
	swipl -g "['tests.lgt']."

run: transform

transform:
	swilgt -g "{ngs_mda_loader},halt."

irun:
	swilgt -g "{ngs_mda_loader},logtalk_load(tools(loader))."
