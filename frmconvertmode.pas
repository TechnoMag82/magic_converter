unit frmConvertMode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, ComCtrls, LCLType, uCmdLineBuilder;

type

  { TConvertModeForm }

  TConvertModeForm = class(TForm)
    BitsRadioButtonNone: TRadioButton;
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
    procedure ChangeSizeCheckBoxChange(Sender: TObject);
    procedure CompressTrackBarChange(Sender: TObject);
    procedure CompressSpinEditChange(Sender: TObject);
    procedure ConvertToComboBoxSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HeightSpinEditChange(Sender: TObject);
    procedure SelectPathToSaveButtonClick(Sender: TObject);
    procedure StartConvertButtonClick(Sender: TObject);
    procedure WidthSpinEditChange(Sender: TObject);
  private
    FCmdLineBuilder: TCmdLineBuilder;
    procedure initDefaults();
    function validateForm(): Boolean;
  public
    constructor Create(AOwner: TComponent; var ACmdLineBuilder: TCmdLineBuilder); overload;
  end;

//var
  //ConvertModeForm: TConvertModeForm;

implementation

{$R *.lfm}

{ TConvertModeForm }

procedure TConvertModeForm.FormCreate(Sender: TObject);
begin
  initDefaults();
end;

procedure TConvertModeForm.ConvertToComboBoxSelect(Sender: TObject);
begin
  with ConvertToComboBox do
  begin
    FCmdLineBuilder.setTargetFormat( Items[ItemIndex] );
    CompressGroupBox.Enabled := (ItemIndex = 0) or (ItemIndex = 1);
    if (CompressGroupBox.Enabled) then
      FCmdLineBuilder.setCompressQuality(CompressSpinEdit.Value)
    else
      FCmdLineBuilder.setCompressQuality(-1);
  end;
end;

procedure TConvertModeForm.BitsRadioButton8Change(Sender: TObject);
begin
  try
    FCmdLineBuilder.setBitsPerPixel(StrToInt((Sender As TRadioButton).Caption));
  except
    FCmdLineBuilder.setBitsPerPixel(-1);
  end;
end;

procedure TConvertModeForm.ChangeSizeCheckBoxChange(Sender: TObject);
begin
  if (ChangeSizeCheckBox.Checked) then
    FCmdLineBuilder.setResize(WidthSpinEdit.Value, HeightSpinEdit.Value)
  else
    FCmdLineBuilder.setResize(-1, -1);
end;

procedure TConvertModeForm.CompressTrackBarChange(Sender: TObject);
begin
  CompressSpinEdit.Value := CompressTrackBar.Position;
  FCmdLineBuilder.setCompressQuality(CompressSpinEdit.Value);
end;

procedure TConvertModeForm.CompressSpinEditChange(Sender: TObject);
begin
  CompressTrackBar.Position := CompressSpinEdit.Value;
  FCmdLineBuilder.setCompressQuality(CompressSpinEdit.Value);
end;

procedure TConvertModeForm.HeightSpinEditChange(Sender: TObject);
begin
  FCmdLineBuilder.setResize(WidthSpinEdit.Value, HeightSpinEdit.Value);
end;

procedure TConvertModeForm.SelectPathToSaveButtonClick(Sender: TObject);
begin
  with TSelectDirectoryDialog.Create(self) do
  begin
    try
      if (execute) then
      begin
        PathToSaveEdit.Text := FileName;
        FCmdLineBuilder.setPath(FileName);
      end;
    finally
      Free;
    end;
  end;
end;

procedure TConvertModeForm.StartConvertButtonClick(Sender: TObject);
begin
  if validateForm() then
  begin
    FCmdLineBuilder.setFileSuffix(FileNameSuffixEdit.Text);
    ModalResult := mrOk;
  end;
end;

procedure TConvertModeForm.WidthSpinEditChange(Sender: TObject);
begin
  FCmdLineBuilder.setResize(WidthSpinEdit.Value, HeightSpinEdit.Value);
end;

procedure TConvertModeForm.initDefaults();
begin
  FCmdLineBuilder.setBitsPerPixel(-1);
  ConvertToComboBoxSelect(ConvertToComboBox);
  ChangeSizeCheckBoxChange(ChangeSizeCheckBox);
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

constructor TConvertModeForm.Create(AOwner: TComponent;
  var ACmdLineBuilder: TCmdLineBuilder);
begin
  FCmdLineBuilder := ACmdLineBuilder;
  inherited Create(AOwner);
end;

end.

