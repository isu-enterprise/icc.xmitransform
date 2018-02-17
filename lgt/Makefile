.PHONY: all tests st itests gt igt t i

all: tests

t: tests

i: itests

tests:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester},halt."

itests:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester}."

gt:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester_gen},halt."

igt:
	LOGTALKHOME=/usr/lib/logtalk/share/logtalk swilgt -g "{tester_gen}."

st:
	swipl -g "['tests.lgt']."
