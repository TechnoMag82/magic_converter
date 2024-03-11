unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ExtCtrls,
  ComCtrls, Buttons, fileutil, LCLType;

type

  { TMainForm }

  TMainForm = class(TForm)
    BitBtn1: TBitBtn;
    FilesListView: TListView;
    MainMenu1: TMainMenu;
    //MenuItem1: TMenuItem;
    //MenuItem2: TMenuItem;
    ExitMenuItem: TMenuItem;
    //MenuItem1: TMenuItem;
    //MenuItem2: TMenuItem;
    //MenuItem3: TMenuItem;
    AboutMenuItem: TMenuItem;
    //MenuItem1: TMenuItem;
    //MenuItem2: TMenuItem;
    //MenuItem3: TMenuItem;
    AddFilesMenuItem: TMenuItem;
    AddFromFolderMenuItem: TMenuItem;
    //MenuItem4: TMenuItem;
    SaveFileListAsMenuItem: TMenuItem;
    OpenFileListMenuItem: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    ProgressBar1: TProgressBar;
    SettingsMenuItem: TMenuItem;
    StatusBar1: TStatusBar;
    procedure AddFromFolderMenuItemClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OpenFileListMenuItemClick(Sender: TObject);
    procedure SaveFileListAsMenuItemClick(Sender: TObject);
  private
    procedure loadFromFolder(path: String);
    procedure addFileToList(filePath: String);
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

procedure TMainForm.AddFromFolderMenuItemClick(Sender: TObject);
var
  selectFolderDialog: TSelectDirectoryDialog;
begin
  selectFolderDialog := TSelectDirectoryDialog.Create(self);
  selectFolderDialog.Title := 'Select folder with images';
  try
    if selectFolderDialog.Execute then
      loadFromFolder(selectFolderDialog.FileName);
  finally
    selectFolderDialog.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
end;

procedure TMainForm.OpenFileListMenuItemClick(Sender: TObject);
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
      files.LoadFromFile(openFileDialog.FileName);
      for filePath in files do
      begin
        addFileToList(filePath);
      end;
    end;
  finally
    files.Free;
    openFileDialog.Free;
  end;
end;

procedure TMainForm.SaveFileListAsMenuItemClick(Sender: TObject);
var
  saveFileDialog: TSaveDialog;
  files: TStringList;
  i: Integer;
begin
  if (FilesListView.Items.Count = 0) then
  begin
    Application.MessageBox('List is empty.', 'Error', MB_OK + MB_ICONWARNING);
    exit;
  end;
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


end.

