unit frmConvertMode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, ComCtrls, LCLType, uCmdLineBuilder;

type

  { TConvertModeForm }

  TConvertModeForm = class(TForm)
    SelectPathToSaveButton: TButton;
    StartConvertButton: TButton;
    ChangeSizeCheckBox: TCheckBox;
    ConvertToComboBox: TComboBox;
    PathToSaveEdit: TEdit;
    FileNameSuffixEdit: TEdit;
    GroupBox1: TGroupBox;
    BitsRadioButton8: TRadioButton;
    BitsRadioButton16: TRadioButton;
    BitsRadioButton24: TRadioButton;
    BitsRadioButton32: TRadioButton;
    CompressGroupBox: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    RadioGroup1: TRadioGroup;
    CompressSpinEdit: TSpinEdit;
    WidthSpinEdit: TSpinEdit;
    HeightSpinEdit: TSpinEdit;
    CompressTrackBar: TTrackBar;
    procedure BitsRadioButton8Change(Sender: TObject);
    procedure CompressTrackBarChange(Sender: TObject);
    procedure CompressSpinEditChange(Sender: TObject);
    procedure ConvertToComboBoxSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SelectPathToSaveButtonClick(Sender: TObject);
    procedure StartConvertButtonClick(Sender: TObject);
    function getCmdLine(): String;
  private
    FCmdLineBuilder: TCmdLineBuilder;
    procedure initDefaults();
    function validateForm(): Boolean;
  public

  end;

//var
  //ConvertModeForm: TConvertModeForm;

implementation

{$R *.lfm}

{ TConvertModeForm }

procedure TConvertModeForm.FormCreate(Sender: TObject);
begin
  FCmdLineBuilder := TCmdLineBuilder.Create;
end;

procedure TConvertModeForm.ConvertToComboBoxSelect(Sender: TObject);
begin
  with ConvertToComboBox do
  begin
    FCmdLineBuilder.setTargetFormat( Items[ItemIndex] );
    CompressGroupBox.Enabled := (ItemIndex = 0) or (ItemIndex = 1);
  end;
end;

procedure TConvertModeForm.BitsRadioButton8Change(Sender: TObject);
begin
  FCmdLineBuilder.setBitsPerPixel((Sender As TRadioButton).Caption);
end;

procedure TConvertModeForm.CompressTrackBarChange(Sender: TObject);
begin
  CompressSpinEdit.Value := CompressTrackBar.Position;
  FCmdLineBuilder.setCompressQuality(IntToStr(CompressSpinEdit.Value));
end;

procedure TConvertModeForm.CompressSpinEditChange(Sender: TObject);
begin
  CompressTrackBar.Position := CompressSpinEdit.Value;
  FCmdLineBuilder.setCompressQuality(IntToStr(CompressSpinEdit.Value));
end;

procedure TConvertModeForm.FormDestroy(Sender: TObject);
begin
  FCmdLineBuilder.Free;
end;

procedure TConvertModeForm.SelectPathToSaveButtonClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(self) do
  begin
    try
      if (execute) then
        PathToSaveEdit.Text := FileName;
    finally
      Free;
    end;
  end;
end;

procedure TConvertModeForm.StartConvertButtonClick(Sender: TObject);
begin
  if validateForm() then
    ModalResult := mrOk;
end;

function TConvertModeForm.getCmdLine(): String;
begin
  Result := FCmdLineBuilder.Build();
end;

procedure TConvertModeForm.initDefaults();
begin
  FCmdLineBuilder.setBitsPerPixel(BitsRadioButton8.Caption);
  with ConvertToComboBox do
  begin
    FCmdLineBuilder.setTargetFormat( Items[ItemIndex] );
    CompressGroupBox.Enabled := (ItemIndex = 0) or (ItemIndex = 1);
  end;
end;

function TConvertModeForm.validateForm(): Boolean;
begin
  Result := false;
  if (ChangeSizeCheckBox.Checked) then
  begin
    if (WidthSpinEdit.Value = 0) or (HeightSpinEdit.Value = 0) then
    begin
      Application.MessageBox('Width and Height must be more than zero.',
        'Error', MB_OK + MB_ICONWARNING);
      exit;
    end;
  end;
  if (not DirectoryExists(PathToSaveEdit.Text)) then
  begin
    Application.MessageBox('Select directory for save converted files.',
        'Error', MB_OK + MB_ICONWARNING);
    exit;
  end;
  Result := true;
end;

end.

