.PHONY: all tests st itests gt igt t i transform run irun

LGT=LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt

all: tests

t: tests

i: itests

tests:
	$(LGT) -g "{tester},halt."

itests:
	$(LGT) -g "{tester},logtalk_load(tools(loader))."

gt:
	$(LGT) -g "{tester_gen},halt."

igt:
	$(LGT) -g "{tester_gen}."

st:
	swipl -g "['tests.lgt']."

run: transform

transform:
	$(LGT) -g "{ngs_mda_loader},halt."

irun:
	$(LGT) -g "{ngs_mda_loader},logtalk_load(tools(loader))."
