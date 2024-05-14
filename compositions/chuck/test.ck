SndBuf test => dac;
"ding.wav" => test.read;

0.5 => test.gain;
0 => test.pos;
1 => test.rate;

for (0 => int i; i < 20; ++i) {
  1::second => now;
  0 => test.pos;
}