unit uConfigIni;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type

  { cConfigIni class }

  cConfigIni = class(TIniFile)
  private

  protected

  public
    function ReadString(const Section, Ident, Default: string): string; override;
    { Reads a string.

      When a Key is missing default value is writed in the file.}
  end;

implementation

{ cConfigIni }

function cConfigIni.ReadString(const Section, Ident, Default: string): string;
begin
  Result := inherited ReadString(Section, Ident, '');
  if Result = '' then
  begin
    Result := Default;
    WriteString(Section, Ident, Default);
  end;
end;

end.

