.PHONY: all tests st itests

all: tests

tests:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester},halt."

itests:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester}."

st:
	swipl -g "['tests.lgt']."
