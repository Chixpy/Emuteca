{ This file is part of Emuteca

  Copyright (C) 2006-2012 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{ cTranslator unit }
unit uTranslator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, strutils;

type

  { Entity that translates texts and component captions.

  The property Section must be set before calling Translate methods.
  }
  cTranslator = class
  private
    FSection: String;
    OpenedFile: boolean;

    IniFile: TCustomIniFile;
    procedure SetSection(const AValue: String);
  public
    property Section: String read FSection write SetSection;
    {< Current internal section.

    This property contains the current section of the .ini file where the
    translation key must be searched.
    }

    function Translate(const Key, sDefault: String): String; overload;
    {< Translates a string.

    Searches the Key, in the current Section, for a translation of the text.

    If the Key is not found, then it will be created in the opened .ini file
    with the value of sDefault.

    @param Key Key where translation is stored.
    @param sDefault Default text.
    @return String translated.
    }
    function Translate(aComponent: TComponent): String; overload;
    {< Translates a component caption and all its children.

    @param aComponent Component wich its text or caption will be translated.
    }

    constructor Create(const aFileName: String); overload;
    {< Creates a cTranslator object.

    Open a .ini file to read the translations.

    @param aFileName The filaname of a .ini with the translations.
    }
    constructor Create(aIniFile: TCustomIniFile); overload;
    {< Creates a cTranslator object.

    Uses a already opened .ini file to read the translations.

    @param aIniFile Opened TIniFile.
    }

    destructor Destroy; override;
  end;

implementation

uses
  FileUtil, Controls, Menus, ActnList, StdCtrls, ExtCtrls;

{ cTranslator }

procedure cTranslator.SetSection(const AValue: String);
begin
  if FSection = AValue then
    Exit;
  FSection := AValue;
end;

function cTranslator.Translate(const Key, sDefault: String): String;
var
  Temp: String;
begin
  Result := IniFile.ReadString(Section, Key, '');
  if Result = '' then
  begin
    Result := sDefault;
    if sDefault = '' then
      Exit;
    Temp := AnsiReplaceText(sDefault, sLineBreak, '\n');
    IniFile.WriteString(Section, Key, Temp);
    IniFile.UpdateFile;
  end
  else
  begin
    Result := AnsiReplaceText(Result, '\n', sLineBreak);
  end;
end;

function cTranslator.Translate(aComponent: TComponent): String;

  procedure TranslateComponent(aComponent: TComponent);
  begin
    if aComponent is TControl then
    begin
      with aComponent as TControl do
      begin
        if not Assigned(Action) then
        begin
          if (Caption <> '-') and (Caption <> '') then
          begin
            Caption := Translate(Name + 'Caption', Caption);
            Hint := Translate(Name + 'Hint', Hint);
          end;
        end;
      end;
      if aComponent is TCustomComboBox then
      begin
        with aComponent as TCustomComboBox do
        begin
          Items.CommaText := Translate(Name + 'Items', Items.CommaText);
        end;
      end
      else if aComponent is TCustomRadioGroup then
      begin
        with aComponent as TCustomRadioGroup do
        begin
          Items.CommaText := Translate(Name + 'Items', Items.CommaText);
        end;
      end;
    end
    else if aComponent is TMenuItem then
    begin
      with aComponent as TMenuItem do
      begin
        if not Assigned(Action) then
        begin
          Caption := Translate(Name + 'Caption', Caption);
          Hint := Translate(Name + 'Hint', Hint);
        end;
      end;
    end
    else if aComponent is TCustomAction then
    begin
      with aComponent as TCustomAction do
      begin
        Caption := Translate(Name + 'Caption', Caption);
        Hint := Translate(Name + 'Hint', Hint);
      end;
    end;
  end;

  //function cTranslator.Translate(aComponent: TComponent): String;
var
  i: integer;
begin
  Result := '';
  TranslateComponent(aComponent);

  i := 0;
  while i < aComponent.ComponentCount do
  begin
    TranslateComponent(aComponent.Components[i]);
    Inc(i);
  end;
end;

constructor cTranslator.Create(const aFileName: String);
begin
  Self.IniFile := TMemIniFile.Create(UTF8ToSys(aFileName));
  OpenedFile := True;
end;

constructor cTranslator.Create(aIniFile: TCustomIniFile);
begin
  Self.IniFile := aIniFile;
  OpenedFile := False;
end;

destructor cTranslator.Destroy;
begin
  if OpenedFile then
    FreeAndNil(IniFile);
  inherited Destroy;
end;

end.

