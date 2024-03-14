unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, Buttons, fileutil, LCLType, ExtDlgs, ActnList,
  frmConvertMode;

type

  { TMainForm }

  TMainForm = class(TForm)
    AboutMenuItem: TMenuItem;
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
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    SettingsMenuItem: TMenuItem;
    StatusBar1: TStatusBar;
    MainToolBar: TToolBar;
    AddFilesToolButton: TToolButton;
    AddFromFolderToolButton: TToolButton;
    SaveListToolButton: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure AddFilesActionExecute(Sender: TObject);
    procedure AddFromFolderActionExecute(Sender: TObject);
    procedure ConvertActionExecute(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenFileListActionExecute(Sender: TObject);
    procedure SaveListAsActionExecute(Sender: TObject);
  private
    procedure loadFromFolder(path: String);
    procedure addFileToList(filePath: String);
    procedure enableActions(enable: Boolean);
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

procedure TMainForm.ConvertActionExecute(Sender: TObject);
begin
  if isListEmpty() then
    exit;
  with TConvertModeForm.Create(self) do
  begin
    try
      ShowModal;
      if ModalResult = mrOk then
      begin
         getCmdLine();
      end;
    finally
      Free;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  AddFilesAction.Hint := AddFilesAction.Caption;
  AddFromFolderAction.Hint := AddFromFolderAction.Caption;
  OpenFileListAction.Hint := OpenFileListAction.Caption;
  SaveListAsAction.Hint := SaveListAsAction.Caption;
  ConvertAction.Hint := ConvertAction.Caption;
end;

procedure TMainForm.OpenFileListActionExecute(Sender: TObject);
var
  openFileDialog: TOpenDialog;
  files: TStringList;
  filePath: String;
begin
  openFileDialog := TOpenDialog.Create(self);
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

procedure TMainForm.enableActions(enable: Boolean);
begin

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

