unit uCmdLineBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lazfileutils;

type
  
  { TCmdLineBuilder }

  TCmdLineBuilder = class
  strict private
    FCompressQuality: ShortInt;
    FTargetFormat: String;
    FBitsPerPixel: ShortInt;
    FWidth, FHeight: Integer;
    FPath: String;
    FSuffix: String;
    FCmdOptions: String;
  public
    procedure setTargetFormat(ATargetFormat: String);
    procedure setCompressQuality(ACompressQuality: ShortInt);
    procedure setBitsPerPixel(ABitsPerPixel: ShortInt);
    procedure setResize(AWidth, AHeight: Integer);
    procedure setPath(APathToSave: String);
    procedure setFileSuffix(AFileSuffix: String);
    procedure prepare();
    function build(inputFile: String): String;
  end;

implementation

{ TCmdLineBuilder }

procedure TCmdLineBuilder.setTargetFormat(ATargetFormat: String);
var
  startPos, endPos: Integer;
begin
  startPos := ATargetFormat.IndexOf('(');
  endPos := ATargetFormat.IndexOf(')');
  FTargetFormat := ATargetFormat.substring(startPos + 1, endPos - startPos - 1);
end;

procedure TCmdLineBuilder.setCompressQuality(ACompressQuality: ShortInt);
begin
  FCompressQuality := ACompressQuality;
end;

procedure TCmdLineBuilder.setBitsPerPixel(ABitsPerPixel: ShortInt);
begin
  FBitsPerPixel := ABitsPerPixel;
end;

procedure TCmdLineBuilder.setResize(AWidth, AHeight: Integer);
begin
  FWidth := AWidth;
  FHeight := AHeight;
end;

procedure TCmdLineBuilder.setPath(APathToSave: String);
begin
  FPath := APathToSave;
end;

procedure TCmdLineBuilder.setFileSuffix(AFileSuffix: String);
begin
  FSuffix := AFileSuffix;
end;

procedure TCmdLineBuilder.prepare();
var
  cmdOptions: TStringList;
begin
  FCmdOptions := '';
  cmdOptions := TStringList.Create;
  cmdOptions.Delimiter := ' ';
  try
    if (FBitsPerPixel > 0) then
      cmdOptions.Add('-depth ' + IntToStr(FBitsPerPixel));
    if (FCompressQuality > 0) then
    begin
      cmdOptions.Add('-quality ' + IntToStr(FCompressQuality));
    end;
    if (FWidth > 0) and (FHeight > 0) then
    begin
      cmdOptions.Add('-resize ' + IntToStr(FWidth) + 'x' + IntToStr(FHeight));
    end;
    FCmdOptions := cmdOptions.DelimitedText.Replace('"', '');
  finally
    cmdOptions.Free;
  end;
end;

function TCmdLineBuilder.build(inputFile: String): String;
begin
  Result := '"' + inputFile + '" ' + FCmdOptions + ' "' + FPath + '/' + ExtractFileNameOnly(inputFile) + FSuffix + FTargetFormat + '"';
end;

end.

