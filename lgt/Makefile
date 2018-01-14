.PHONY: all tests st

all: tests

tests:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tests}."

st:
	swipl -g "['tests.lgt']."
