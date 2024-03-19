unit frmConvert;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  uConvertThread, uCmdLineBuilder;

type

  { TConvertForm }

  TConvertForm = class(TForm)
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FConvertThread: TConvertThread;
    procedure showStatus(Sender: TObject; progressAction: TAction; currentProgress, total: Integer);
  public
    constructor Create(AOwner: TComponent; var ACmdLineBuilder: TCmdLineBuilder;
      var AFiles: TStringList); overload;
  end;

//var
  //ConvertForm: TConvertForm;

implementation

{$R *.lfm}

{ TConvertForm }

procedure TConvertForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  FConvertThread.Terminate;
  //CloseAction := caFree;
end;

procedure TConvertForm.FormCreate(Sender: TObject);
begin
  FConvertThread.Start;
end;

procedure TConvertForm.showStatus(Sender: TObject; progressAction: TAction;
  currentProgress, total: Integer);
begin
  if (progressAction = acInProgress) then
  begin
    Label1.Caption := IntToStr(currentProgress) + ' of ' + IntToStr(total);
    if (ProgressBar1.Max = 0) then
      ProgressBar1.Max := total;
    ProgressBar1.Position := currentProgress;
  end;
  if (progressAction = acCompleted) then
  begin
    ModalResult := mrOk;
  end;
end;

constructor TConvertForm.Create(AOwner: TComponent;
  var ACmdLineBuilder: TCmdLineBuilder; var AFiles: TStringList);
begin
  FConvertThread := TConvertThread.Create(true, ACmdLineBuilder, AFiles);
  FConvertThread.OnShowStatus := @showStatus;
  inherited Create(AOwner);
end;


end.

