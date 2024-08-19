# This is for recording the scripts

## test.oracle.py

```shell
python3 ./scripts/test_oracle.py ./tests/sysy/external_test/00.sy --timeout 600 --opt-level 1 --output-dir ./output --executable-path ./target/release/compiler --no-compile
```

Run all external tests:

```shell
./test_oracle_script.sh 0 (opt level 0)
./test_oracle_script.sh 1 (opt level 1)
```

## execute.py

```shell
python3 ./scripts/execute.py     --timeout 600     --testcase-dir ./tests/sysy/external_test     --output-dir ./output --opt-level 1     --runtime-lib-dir ./tests/sysy/sysy-runtime-lib
```

## special cases
The cases that faced a diff between the stdout and out, the reason is clang(float -> double -> float, so the precision is different). So I add 'f' behind the float number in .fsc files.

- 08(passed)
- 18(error-opt1)
- 25(error-opt1)

## time cost

### opt level 0
```
00:  0H-0M-3S-750081us
01:  0H-0M-1S-759401us
02:  0H-0M-0S-980133us
03:  0H-0M-0S-843us
04:  0H-0M-0S-2245us
05:  0H-0M-0S-463us
06:  0H-0M-0S-31275us
07:  0H-0M-0S-21640us
08:  0H-0M-0S-781us
09:  0H-0M-8S-118058us
10:  0H-0M-7S-837897us
11:  0H-0M-0S-260356us
12:  0H-0M-18S-415172us
13:  0H-0M-18S-876455us
14:  0H-1M-27S-650281us
15:  0H-1M-46S-488651us
16:  0H-1M-2S-459550us
17:  0H-0M-28S-500107us
18:  0H-1M-22S-697408us
19:  0H-2M-1S-1842us
20:  0H-0M-34S-775036us
21:  0H-0M-31S-933386us
22:  0H-0M-20S-946460us
23:  0H-0M-8S-630777us
24:  0H-0M-11S-549536us
25:  0H-0M-4S-13585us
26:  0H-1M-2S-353389us
27:  0H-0M-0S-297447us
28:  0H-0M-18S-909993us
```

### opt level 1
```
00:  0H-0M-2S-82603us
01:  0H-0M-0S-333399us
02:  0H-0M-0S-606027us
03:  0H-0M-0S-1173us
04:  0H-0M-0S-4294us
05:  0H-0M-0S-6026us
06:  0H-0M-0S-18852us
07:  0H-0M-0S-18416us
08:  0H-0M-0S-1483us
09:  0H-0M-4S-540380us
10:  0H-0M-0S-788011us
11:  0H-0M-0S-31414us
12:  0H-0M-12S-831224us
13:  0H-0M-10S-173699us
14:  0H-0M-51S-698095us
15:  0H-1M-16S-930845us
16:  0H-0M-36S-97474us
17:  0H-0M-24S-307214us
18:  0H-1M-33S-944031us
19:  0H-1M-21S-669783us
20:  0H-0M-25S-325013us
21:  0H-0M-22S-219833us
22:  0H-0M-14S-312134us
23:  0H-0M-5S-515963us
24:  0H-0M-4S-833656us
25:  0H-0M-2S-950408us
26:  0H-0M-55S-384622us
27:  0H-0M-0S-277734us
28:  0H-0M-15S-370368us
```