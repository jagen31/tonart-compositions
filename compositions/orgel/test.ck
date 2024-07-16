MidiOut mout;
mout.open("IAC Driver Bus 1");
MidiMsg msg;
0x90 => msg.data1;
0x3D => msg.data2;
0x78 => msg.data3;

while(true) {
    1::second => now;
    <<< "sending" >>>;
    <<< msg >>>;
    mout.send(msg);
}