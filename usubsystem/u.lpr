program USubsystemProgram;
{$mode delphi} {$h+}

uses
 Classes,Process,SysUtils;

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
 Qemu:TTargetPlatform;

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
