program ProjectOne;
uses
 GlobalConst,
 GlobalTypes,
 Threads,
 Console,
 Framebuffer,
 BCM2836,
 BCM2709,
 Crt;

//uses Crt,SysUtils;
//begin
//WriteLn('ProjectOne');
//while True do
// Sleep(1*1000);
//end.

//var
// WindowHandle:TWindowHandle;

begin
 WriteLn('ProjectOne');
// WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
// ConsoleWindowWriteLn(WindowHandle,'Hello Ultibo!');
 ThreadHalt(0);
end.
