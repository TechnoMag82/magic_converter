unit uConvertThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCmdLineBuilder
  {$IFOPT D+}, MultiLog, filechannel{$ENDIF};

type
  TAction = (acInProgress, acCompleted);
  TShowStatusEvent = procedure (Sender: TObject; Action: TAction; currentPorgress, total: Integer) of object;
  { TConvertThread }

  TConvertThread = class(TThread)
  private
     FCmdLineBuilder: TCmdLineBuilder;
     FFiles: TStringList;
     FOnShowStatus: TShowStatusEvent;
     FCurrent: Integer;
     FTotal: Integer;
  protected
     procedure Execute; override;
     procedure sendEvent();
     procedure sendCompletedEvent();
  public
     constructor Create(const ASuspended: boolean; var ACmdLineBuilder: TCmdLineBuilder;
       var AFiles: TStringList);
     property OnShowStatus: TShowStatusEvent read FOnShowStatus write FOnShowStatus;
  end;

implementation

{ TConvertThread }

procedure TConvertThread.sendEvent();
begin
  if Assigned(FOnShowStatus) then
    FOnShowStatus(self, acInProgress, FCurrent, FTotal);
end;

procedure TConvertThread.sendCompletedEvent();
begin
  if Assigned(FOnShowStatus) then
    FOnShowStatus(self, acCompleted, FCurrent, FTotal);
end;

constructor TConvertThread.Create(const ASuspended: boolean;
  var ACmdLineBuilder: TCmdLineBuilder; var AFiles: TStringList);
begin
  FreeOnTerminate := true;
  FCmdLineBuilder := ACmdLineBuilder;
  FFiles := AFiles;
  FTotal := AFiles.Count;
  FCurrent := 0;
  {$IFOPT D+}
  Logger.Channels.Add(TFileChannel.Create('debug.log'));
  {$ENDIF}
  inherited Create(ASuspended);
end;

procedure TConvertThread.Execute;
var
  FFile: String;
begin
  FCmdLineBuilder.Prepare();
  for FFile in FFiles do
  begin
    if (Terminated) then
      break;
    Inc(FCurrent);
    {$IFOPT D+}
    Logger.Send(FCmdLineBuilder.build(FFile));
    {$ENDIF}
    Synchronize(@SendEvent);
    ExecuteProcess('/usr/bin/convert', FCmdLineBuilder.build(FFile), []);
  end;
  Synchronize(@SendCompletedEvent);
end;

end.

