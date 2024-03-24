unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, Buttons, fileutil, LCLType, ExtDlgs, ActnList,
  frmConvertMode,
  frmConvert,
  frmAbout,
  uCmdLineBuilder;

type

  { TMainForm }

  TMainForm = class(TForm)
    AboutMenuItem: TMenuItem;
    ClearListAction: TAction;
    ConvertAction: TAction;
    AddFilesMenuItem: TMenuItem;
    AddFromFolderMenuItem: TMenuItem;
    OpenFileListMenuItem: TMenuItem;
    SaveFileListAsMenuItem: TMenuItem;
    SaveListAsAction: TAction;
    OpenFileListAction: TAction;
    AddFromFolderAction: TAction;
    AddFilesAction: TAction;
    ActionList1: TActionList;
    FilesListView: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    ToolbarImageList: TImageList;
    MainMenu1: TMainMenu;
    ExitMenuItem: TMenuItem;
    Panel1: TPanel;
    SettingsMenuItem: TMenuItem;
    StatusBar1: TStatusBar;
    MainToolBar: TToolBar;
    AddFilesToolButton: TToolButton;
    AddFromFolderToolButton: TToolButton;
    SaveListToolButton: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure AddFilesActionExecute(Sender: TObject);
    procedure AddFromFolderActionExecute(Sender: TObject);
    procedure ClearListActionExecute(Sender: TObject);
    procedure ConvertActionExecute(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OpenFileListActionExecute(Sender: TObject);
    procedure SaveListAsActionExecute(Sender: TObject);
  private
    FCmdLineBuilder: TCmdLineBuilder;
    FFilesToConvert: TStringList;
    procedure loadFromFolder(path: String);
    procedure addFileToList(filePath: String);
    procedure rebuildFilesList();
    procedure startConvertForm();
    procedure clearList();
    function isListEmpty(): Boolean;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.AddFilesActionExecute(Sender: TObject);
var
  openFilesDialog: TOpenPictureDialog;
  filePath: String;
begin
  openFilesDialog := TOpenPictureDialog.Create(self);
  openFilesDialog.Title := 'Select images to add';
  openFilesDialog.Filter := 'All files|*.*|JPEG images|*.jpg|PNG images|*.png';
  openFilesDialog.FilterIndex := 0;
  openFilesDialog.Options := [ofAllowMultiSelect, ofReadOnly, ofFileMustExist, ofNoNetworkButton];
  try
    if openFilesDialog.Execute then
    begin
      for filePath in openFilesDialog.Files do
      begin
        addFileToList(filePath);
      end;
      StatusBar1.SimpleText := 'Итого: ' + IntToStr(FilesListView.Items.Count);
    end;
  finally
    openFilesDialog.Free;
  end;
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  TAboutForm.Create(self).ShowModal;
end;

procedure TMainForm.AddFromFolderActionExecute(Sender: TObject);
var
  selectFolderDialog: TSelectDirectoryDialog;
begin
  selectFolderDialog := TSelectDirectoryDialog.Create(self);
  selectFolderDialog.Title := 'Select folder with images';
  try
    if selectFolderDialog.Execute then
    begin
      loadFromFolder(selectFolderDialog.FileName);
      StatusBar1.SimpleText := 'Итого: ' + IntToStr(FilesListView.Items.Count);
    end;
  finally
    selectFolderDialog.Free;
  end;
end;

procedure TMainForm.ClearListActionExecute(Sender: TObject);
begin
  if (Application.MessageBox('Do you want to clear list and start new convert?',
        'Convert', MB_YESNO + MB_ICONQUESTION) = IDYES) then
    clearList();
end;

procedure TMainForm.ConvertActionExecute(Sender: TObject);
begin
  if isListEmpty() then
    exit;
  with TConvertModeForm.Create(self, FCmdLineBuilder) do
  begin
    try
      ShowModal;
      if ModalResult = mrOk then
      begin
        rebuildFilesList;
        startConvertForm;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCmdLineBuilder := TCmdLineBuilder.Create;
  FFilesToConvert := TStringList.Create;
  Caption := Application.Title;
  AddFilesAction.Hint := AddFilesAction.Caption;
  AddFromFolderAction.Hint := AddFromFolderAction.Caption;
  OpenFileListAction.Hint := OpenFileListAction.Caption;
  SaveListAsAction.Hint := SaveListAsAction.Caption;
  ConvertAction.Hint := ConvertAction.Caption;
  ClearListAction.Hint := ClearListAction.Caption;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FCmdLineBuilder.Free;
  FFilesToConvert.Free;
end;

procedure TMainForm.OpenFileListActionExecute(Sender: TObject);
var
  openFileDialog: TOpenDialog;
  files: TStringList;
  filePath: String;
begin
  openFileDialog := TOpenDialog.Create(self);
  openFileDialog.Filter := 'Text files|*.txt';
  openFileDialog.FilterIndex := 0;
  try
    if openFileDialog.Execute then
    begin
      FilesListView.Clear;
      files := TStringList.Create;
      try
        files.LoadFromFile(openFileDialog.FileName);
        for filePath in files do
        begin
          addFileToList(filePath);
        end;
        StatusBar1.SimpleText := 'Итого: ' + IntToStr(FilesListView.Items.Count);
      finally
        files.Free;
      end;
    end;
  finally
    openFileDialog.Free;
  end;
end;

procedure TMainForm.SaveListAsActionExecute(Sender: TObject);
var
  saveFileDialog: TSaveDialog;
  files: TStringList;
  i: Integer;
begin
  if isListEmpty() then
    exit;
  saveFileDialog := TSaveDialog.Create(self);
  saveFileDialog.Title := 'Select file for save';
  try
    if saveFileDialog.Execute then
    begin
      files := TStringList.Create;
      for i := 0 to FilesListView.Items.Count - 1 do
      begin
        files.Add(FilesListView.Items[i].SubItems[0]);
      end;
      files.SaveToFile(saveFileDialog.FileName);
    end;
  finally
    saveFileDialog.Free;
    files.Free;
  end;
end;

procedure TMainForm.loadFromFolder(path: String);
var
  files: TStringList;
  filePath: String;
begin
  files := TStringList.Create;
  try
    FindAllFiles(files, path, '*.jpg;*.bmp;*.xmp;*.png', true);
    for filePath in files do
    begin
      addFileToList(filePath);
    end;
    StatusBar1.SimpleText := 'Итого: ' + IntToStr(FilesListView.Items.Count);
  finally
    files.Free;
  end;
end;

procedure TMainForm.addFileToList(filePath: String);
var
  listItem: TListItem;
begin
  listItem := FilesListView.Items.Add;
  listItem.Caption := ExtractFileName(filePath);
  listItem.SubItems.Add(filePath);
end;

procedure TMainForm.rebuildFilesList();
var
  listItem: TListItem;
begin
  FFilesToConvert.Clear;
  for listItem in FilesListView.Items do
  begin
     FFilesToConvert.add(listItem.SubItems[0]);
  end;
end;

procedure TMainForm.startConvertForm();
var
  convertForm: TConvertForm;
begin
  convertForm := TConvertForm.Create(self, FCmdLineBuilder, FFilesToConvert);
  try
    convertForm.ShowModal;
    if (convertForm.ModalResult = mrOk) then
    begin
      if (Application.MessageBox('Conversion is completely completed. Do you want to start new convert?',
        'Convert', MB_YESNO + MB_ICONINFORMATION) = IDYES) then
        begin
          clearList();
        end;
    end else if (convertForm.ModalResult = mrCancel) then
    begin
      Application.MessageBox('Conversion is aborted!',
        'Convert', MB_OK + MB_ICONEXCLAMATION);
    end;
  finally
    convertForm.Free;
  end;
end;

procedure TMainForm.clearList();
begin
  FilesListView.Clear;
  FFilesToConvert.Clear;
  StatusBar1.SimpleText := '';
end;

function TMainForm.isListEmpty() : Boolean;
begin
  if (FilesListView.Items.Count = 0) then
  begin
    Application.MessageBox('List is empty.', 'Error', MB_OK + MB_ICONWARNING);
    Result := True;
  end else
    Result := False;
end;

end.

