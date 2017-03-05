program ProjectOne;
uses
 GlobalConst,
 GlobalTypes,
 Threads,
 Console,
 Framebuffer,
 BCM2836,
 BCM2709;

//uses Crt,SysUtils;
//begin
//Write('ProjectOne');
//while True do
// Sleep(1*1000);
//end.

//{$mode objfpc}{$H+}

var
 WindowHandle:TWindowHandle;

begin
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
 ConsoleWindowWriteLn(WindowHandle,'Hello Ultibo!');
 ThreadHalt(0);
end.
