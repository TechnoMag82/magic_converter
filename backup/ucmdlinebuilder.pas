unit uCmdLineBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  
  { TCmdLineBuilder }

  TCmdLineBuilder = class
  strict private
    FCompressQuality: String;
    FTargetFormat: String;
    FBitsPerPixel: String;
  public
    constructor Create;
    procedure setTargetFormat(ATargetFormat: String);
    procedure setCompressQuality(ACompressQuality: String);
    procedure setBistPerPixel(ABitsPerPixel: String);
    function Build(): String;
  end;

implementation

{ TCmdLineBuilder }

constructor TCmdLineBuilder.Create;
begin
end;

procedure TCmdLineBuilder.setTargetFormat(ATargetFormat: String);
begin
  FTargetFormat := ATargetFormat;
end;

procedure TCmdLineBuilder.setCompressQuality(ACompressQuality: String);
begin
  FCompressQuality := ACompressQuality;
end;

procedure TCmdLineBuilder.setBitsPerPixel(ABitsPerPixel: String);
begin
  FBitsPerPixel := ABitsPerPixel;
end;

function TCmdLineBuilder.Build(): String;
var
  cmdOptions: TStringList;
begin
  Result := '';
end;

end.

