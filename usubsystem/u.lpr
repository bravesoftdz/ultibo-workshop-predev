program USubsystemProgram;
{$mode delphi} {$h+}

uses
 Classes,FpSock,Process,SSockets,SysUtils;

type
 TArrayOfString = array of String;
 TTargetPlatform = class
  Options:array of String;
  Config,Symbol:String;
  constructor Create(Options:array of String;Config,Symbol:String);
  procedure Build(ProgramFile,BuildDir:String);
 end;

const
 UltiboInstallation='/root/ultibo/core/fpc';

var
 FrameNumber:Integer;
 Qemu:TTargetPlatform;
// QemuMonitor:TTcpChannel;
 QemuProcess:TProcess;
 QemuStopping:Boolean;
// SerialChannels:array [0..3] of TTcpChannel;

procedure Log(Message:String);
begin
 WriteLn(Message);
end;

function Path(Parts:array of String):String;
var
 I:Integer;
begin
 Result:=Parts[Low(Parts)];
 for I:=Low(Parts) + 1 to High(Parts) do
  Result:=Result + '/' + Parts[I];
end;

function WhichExecutable(ProgramName:String):String;
begin
 Result:='';
end;

constructor TTargetPlatform.Create(Options:array of String;Config,Symbol:String);
var
 I:Integer;
begin
 SetLength(self.Options,Length(Options));
 for I:=Low(Options) to High(Options) do
  self.Options[I]:=Options[I];
 self.Config:=Config;
 self.Symbol:=Symbol;
end;

function Option(Name,Value:String):String;
begin
 Result:=Name + Value;
end;

procedure Fail(Condition:Boolean);
begin
 if not Condition then
  raise Exception.Create('Fail');
end;

procedure RecreateDir(Dir:String);
var
 FileRec:TSearchRec;
 P:String;
begin
 if FileExists(Dir) then
  begin
   if FindFirst(Path([Dir,'*']),faAnyFile,FileRec) = 0 then
   begin
    repeat
     P:=Path([Dir,FileRec.Name]);
     if FileGetAttr(P) and faDirectory = 0 then
      Fail(DeleteFile(P));
    until FindNext(FileRec) <> 0;
    FindClose(FileRec);
    Fail(RemoveDir(Dir));
   end;
  end;
 Fail(CreateDir(Dir));
end;

procedure TestProcess;
var
 P:TProcess;
 L:TStringList;
begin
 P:=TProcess.Create(nil);
 P.Executable:='/bin/ls';
 P.Options:=P.Options + [poWaitOnExit,poUsePipes];
 P.Execute;
 Log(Format('exit status %d',[P.ExitStatus]));
 L:=TStringList.Create;
 L.LoadFromStream(P.Output);
 L.SaveToFile('testoutput.txt');
 L.Free;
 P.Free;
end;

procedure CopyFile(Source,Destination:String);
var
 S:TMemoryStream;
begin
 S:=TMemoryStream.Create;
 try
  S.LoadFromFile(Source);
  S.SaveToFile(Destination);
 finally
  S.Free;
 end;
end;

procedure StartQemu;
var
 I:Integer;
 Parts:TStringList;
 Command:String;
begin
 QemuProcess:=TProcess.Create(nil);
{
 Qemu.(["qemu-system-arm",
                                 "-M", "versatilepb",
                                 "-cpu", "cortex-a8",
                                 "-kernel", "kernel.bin",
                                 "-m", "1024M",
                                 "-usb",
                                 "-display", "none",
                                 "-monitor", "tcp:127.0.0.1:38004,server",
                                 "-serial", "tcp:127.0.0.1:38000,server",
                                 "-serial", "tcp:127.0.0.1:38001,server",
                                 "-serial", "tcp:127.0.0.1:38002,server",
                                 "-serial", "tcp:127.0.0.1:38003,server"],


                                stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE)
}
end;

procedure TTargetPlatform.Build(ProgramFile,BuildDir:String);
var
 I:Integer;
 Fpc:TProcess;
 Parts:TStringList;
 Command:String;
begin
 Log(Format('Build %s into %s',[ProgramFile,BuildDir]));
 RecreateDir(BuildDir);
 RecreateDir('artifacts');

 Parts:=TStringList.Create;
 Parts.Add('fpc');
 Parts.Add('-d' + Symbol);
 Parts.Add('-l-');
 Parts.Add('-v0ewn');
 Parts.Add('-B');
 Parts.Add('-Tultibo');
 Parts.Add('-O2');
 Parts.Add('-Parm');
 Parts.Add(Option('-FE',BuildDir));
 for I:=Low(Options) to High(Options) do
  Parts.Add(Options[I]);
 Parts.Add(Option('@',Path([UltiboInstallation,'bin',Config])));
 Parts.Add(ProgramFile);
 Command:=Parts[0];
 for I:=1 to Parts.Count - 1 do
  Command:=Command + ' ' + Parts[I];
 Parts.Free;

 Fpc:=TProcess.Create(nil);
 Fpc.Options:=Fpc.Options + [poWaitOnExit]; //,poUsePipes];
 Fpc.Executable:='/bin/bash';
 Fpc.Parameters.Add('-c');
 Fpc.Parameters.Add(Command);

// Log(Fpc.Executable);
// for I:=0 to Fpc.Parameters.Count - 1 do
//  Log(Fpc.Parameters[I]);
 Fpc.Execute;
 if Fpc.ExitStatus = 0 then
  CopyFile('kernel.bin','artifacts/kernel.bin')
 else
  Log(Format('exit status %d',[Fpc.ExitStatus]));
 Fpc.Free;

 //Log('mv kernel* $LPR_FOLDER/$OUTPUT');
 //Log('if [[ $? -ne 0 ]]; then log fail: $?; fi');
 //Log('local THISOUT=$OUTPUT/kernels-and-tests/$FOLDER');
 //Log('rm -rf $THISOUT && \');
 //Log('//        mkdir -p $THISOUT && \');
 //Log('//        cp -a $FOLDER/$OUTPUT/* $THISOUT && \');
 //Log('//        if [[ $? -ne 0 ]]; then log fail: $?; fi');
end;

procedure Build;
begin
 Qemu:=TTargetPlatform.Create
  (['-CpARMV7A','-WpQEMUVPB'],'qemuvpb.cfg','TARGET_QEMUARM7A');
// RPi:=TTargetPlatform.Create
//  (['-CpARMV6','-WpRPIB'],'rpi.cfg','TARGET_RPI_INCLUDING_RPI0');
//RPi2:=TTargetPlatform.Create
//  (['-CpARMV7A','-WpRPI2B'],'rpi2.cfg','TARGET_RPI2_INCLUDING_RPI3');
// RPi3:=TTargetPlatform.Create
//  (['-CpARMV7A','-WpRPI3B'],'rpi3.cfg','TARGET_RPI3');

Qemu.Build('projects/projectone/projectone.lpr','obj');
//TestProcess;
end;

procedure Bootstrap;
begin
end;

procedure Main;
var s:String;
begin
 if ParamCount = 1 then
  begin
   s:=ParamStr(1);
   if s = 'bootstrap' then
    Bootstrap
   else if s = 'build' then
    Build
   else
    Log(Format('unrecognized %s',[s]));
  end
 else
  begin
   Log('need one argument');
  end;
end;

begin
 Main;
end.

OUTPUT=build-output
ERRORS=$OUTPUT/build-errors.txt
LOG=$OUTPUT/build.log
rm -rf $OUTPUT
mkdir -p $OUTPUT
rm -f $LOG

function create-build-summary {
    cat $LOG | egrep -i '(fail|error|warning|note):' | sort | uniq > $ERRORS
    log
    log Summary:
    log
    cat $ERRORS | tee -a $LOG
    log
    log $(wc $ERRORS)
    if [[ -s $ERRORS ]]
        exit 1

function unix_line_endings {
    tr -d \\r < $1 > tmp && \
    mv tmp $1

function convert-frames {
    convert-frames-by-size 1024x768
    convert-frames-by-size 1920x1080
    convert-frames-by-size 1920x1200

function convert-frames-by-size {
    local SIZE=$1
    local FRAMES=run-qemu-output/frame*-${SIZE}x3.fb
    ls $FRAMES > /dev/null 2>&1
    if [[ $? -eq 0 ]]
        for frame in $FRAMES
            ultibo-bash convert -size $SIZE -depth 8 rgb:$frame ${frame%.fb}.png
            rm $frame

function test-qemu-target {
    local FOLDER=$1
    cd $FOLDER/$OUTPUT
    time python $RESTORE_PWD/run-qemu
    for textfile in run-qemu-output/*.txt
        unix_line_endings $textfile
    sed -i 's/.\x1b.*\x1b\[D//' run-qemu-output/qemumonitor.txt
    sed -i 's/\x1b\[K//' run-qemu-output/qemumonitor.txt
    ls run-qemu-output/screen*.ppm > /dev/null 2>&1
    if [[ $? -eq 0 ]]
        for screen in run-qemu-output/screen*.ppm
            ultibo-bash convert $screen ${screen%.ppm}.png && \
            rm $screen
    convert-frames
    file run-qemu-output/*



outputfolder = 'run-qemu-output'
class TcpChannel:
    def __init__ (self, name, port):
#       print name, port
        self.name = name
        self.port = port
        self.linebuffer = ""
        self.socket = socket.socket ()
        opened = False
        while not opened:
            try:
                self.socket.connect (('127.0.0.1', self.port))
                opened = True
            except:
                sleep (0.1)
        self.logfile = open ('{}/{}.txt'.format (outputfolder, self.name), 'a')
    def writeline (self, line, echo=False):
#       print '{}>>>>{}'.format (self.name, line)
        if echo:
            self.logfile.write ('{}>>>>{}\n'.format (self.name, line))
        self.socket.sendall (line + '\n')
    def writekeyboardline (self, line, echo=False):
#       print '{}>>>>{}'.format (self.name, line)
        if echo:
            self.logfile.write ('{}>>>>{}\n'.format (self.name, line))
        self.socket.sendall (line + '\r')
    def drain (self):
#       print 'drain {}'.format (self.name)
            while ([], [], []) != select.select ([self.socket], [], [], 0):
                try:
                    data = self.socket.recv (4096)
                except:
                    break
                if data == "":
                    break
                self.logfile.write (data)
                for c in data:
                    if c == '\r':
                        pass
                    elif c == '\n':
#                       print '{}<<<<{}'.format(self.name, self.linebuffer)
                        index = self.linebuffer.find ('program stop')
                        if index != -1:
#                           print self.linebuffer
                            global stopping
                            stopping = True
                            monitor.writeline ('quit')
                        index = self.linebuffer.find ('frame buffer')
                        if index != -1:
#                           print self.linebuffer
                            fb = parse.parse ('{} frame buffer at {address} size {width:d}x{height:d}x{depth:d}', self.linebuffer)
                            if fb:
                                framecapture (fb.named ['address'], fb.named ['width'], fb.named ['height'], fb.named ['depth'])
                        self.linebuffer = ""
                    else:
                        self.linebuffer = self.linebuffer + c
                sleep (0.001, self.name)
    def finishquit (self, process):
        while process.returncode == None:
            sleep (0.001, 'finishquit ' + self.name)
            process.poll ()
            if ([], [], []) != select.select ([self.socket], [], [], 0):
                sys.stdout.write (self.socket.recv (4096))
    def close (self):
        self.logfile.close ()
        self.socket.close ()

procedure DrainAll;
var
 I:Integer;
begin
 for I:=Low(SerialChannels) to High(SerialChannels) do
  SerialChannels[I].Drain;
 QemuMonitor.Drain;
end;

procedure FrameCapture(Address,Width,Height,Depth:Integer);
begin
 print 'framecapture', address, width, height, depth
 monitor.writeline ('memsave {} {} {}/frame-{:02d}-{}x{}x{}.fb'.format (address, width * height * depth, outputfolder, framenumber, width, height, depth))
 Inc(FrameNumber);
end;

procedure QemuWork;
begin
 QemuStopping:=False;
 while not QemuStopping do
  Drainall('not stopping');
 Drainall ('stopping');
 Sleep(1*1000);
 Drainall ('one more before quit');
 QemuMonitor.FinishQuit(QemuProcess);
end;

RunQemuTest;
begin
 RecreateDir(QemuOutputFolder);
 StartQemu;
 FrameNumber:=1;
 QemuMonitor:=TcpChannel.Create('qemumonitor', 38004);
 for I:=Low(SerialChannels) to High(SerialChannels) do
  SerialChannels[I]:=TcpChannel.Create(Format('serial%d',[38000 + I]));
 QemuWork;
 QemuMonitor.Close;
 for I:=Low(SerialChannels) to High(SerialChannels) do
  SerialChannels[I].Close;
end;

#screennumber = 1
#def screendump ():
#    global screennumber
#    monitor.writeline ('screendump {}/screen-{:02d}.ppm'.format (outputfolder, screennumber))
#    screennumber = screennumber + 1
