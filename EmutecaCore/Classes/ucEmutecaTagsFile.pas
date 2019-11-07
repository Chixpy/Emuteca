unit ucEmutecaTagsFile;

{< cEmutecaTagsFile class unit.

  This file is part of Emuteca Core.

  Copyright (C) 2006-2018 Chixpy

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
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, LazUTF8, LazFileUtils,
  // CHX abstract classes
  uaCHXStorable;

const
  krsSectionBegin = '[';
  krsSectionEnd = ']';

type

  { cEmutecaTagsFileSection }

  cEmutecaTagsFileSection = class(TComponent)

  private
    FLines: TStringList;
    FSectionName: string;
    procedure SetSectionName(const AValue: string);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property SectionName: string read FSectionName write SetSectionName;

    property Lines: TStringList read FLines;

  end;

  cEmutecaGenTagsFileSectionList = specialize
    TFPGObjectList<cEmutecaTagsFileSection>;

  cEmutecaTagsFileSectionList = class(cEmutecaGenTagsFileSectionList)
  end;

  cEmutecaTagsFile = class(caCHXStorableTxt)
  private
    FSectionBegin: string;
    FSectionEnd: string;
    FSections: cEmutecaTagsFileSectionList;
    procedure SetSectionBegin(const AValue: string);
    procedure SetSectionEnd(const AValue: string);

  protected
    procedure ParseStrLst(aTxtFile: TStrings);

  public
    procedure LoadFromStrLst(aTxtFile: TStrings); override;
    procedure SaveToStrLst(aTxtFile: TStrings); override;

    procedure SectionNameList(aStrList: TStrings);
    function SectionByName(aSectionName: string): cEmutecaTagsFileSection;
    {< Returns the section with aSectionName name, @nil if not found.

    The text encountered before any section is in the first "virtual" section,
      under the name '' (empty string).
    }

    procedure AddGroup(const aSectionName: string; const aGroupID: string);
    procedure RemoveGroup(const aSectionName: string; const aGroupID: string);

    procedure ANDTagsFile(aFilename: string);
    procedure ANDTagsFile(aStrList: TStrings);
    {< Performs an AND operation with another cEmutecaTagsFile.

      In other words, only keep sections and IDs that are in both files.
    }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Sections: cEmutecaTagsFileSectionList read FSections;

    property SectionBegin: string read FSectionBegin write SetSectionBegin;
    property SectionEnd: string read FSectionEnd write SetSectionEnd;

  end;

{< Defines a special ini file type for tags. }

implementation

{ cEmutecaTagsFile }

constructor cEmutecaTagsFile.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FSections := cEmutecaTagsFileSectionList.Create(True);

  SectionBegin := krsSectionBegin;
  SectionEnd := krsSectionEnd;
end;

destructor cEmutecaTagsFile.Destroy;
begin
  Sections.Free;
  inherited Destroy;
end;

procedure cEmutecaTagsFile.SetSectionBegin(const AValue: string);
begin
  if FSectionBegin = AValue then
    Exit;
  FSectionBegin := AValue;
end;

procedure cEmutecaTagsFile.SetSectionEnd(const AValue: string);
begin
  if FSectionEnd = AValue then
    Exit;
  FSectionEnd := AValue;
end;

procedure cEmutecaTagsFile.ParseStrLst(aTxtFile: TStrings);
var
  CurrSection: cEmutecaTagsFileSection;
  i: integer;
  CurrLine: string;
begin
  // Strings before any SectionByName. Internally managed as a Section
  //   with empty name at position 0 in section list.
  CurrSection := SectionByName('');

  if not assigned(CurrSection) then
  begin
    CurrSection := cEmutecaTagsFileSection.Create(self);
    CurrSection.SectionName := '';
    Sections.Add(CurrSection);
  end;

  i := 0;
  while i < aTxtFile.Count do
  begin
    CurrLine := UTF8Trim(aTxtFile[i]);

    // Ignoring empty lines
    if CurrLine <> '' then
    begin
      // Is it a Section?
      if UTF8StartsText(SectionBegin, CurrLine) and
        UTF8EndsText(SectionEnd, CurrLine) then
      begin
        // It's a Section

        // Extracting Section Name
        CurrLine := UTF8Trim(UTF8Copy(CurrLine, UTF8Length(SectionBegin) +
          1, UTF8Length(CurrLine) - 1 - UTF8Length(SectionBegin)));

        // Already exists?
        CurrSection := SectionByName(CurrLine);

        if not assigned(CurrSection) then
        begin
          CurrSection := cEmutecaTagsFileSection.Create(self);
          CurrSection.SectionName := CurrLine;
          Sections.Add(CurrSection);
        end;
      end
      else
      begin
        // It's a group ID
        CurrSection.Lines.Add(CurrLine);
      end;
    end;
    Inc(i);
  end;
end;

procedure cEmutecaTagsFile.LoadFromStrLst(aTxtFile: TStrings);
begin
  Sections.Clear;

  ParseStrLst(aTxtFile);
end;

procedure cEmutecaTagsFile.SaveToStrLst(aTxtFile: TStrings);
var
  i: integer;
  CurrSection: cEmutecaTagsFileSection;
begin
  aTxtFile.BeginUpdate;
  try
    i := 0;
    while i < Sections.Count do
    begin
      CurrSection := Sections[i];

      if (i <> 0) or (CurrSection.SectionName <> '') then
        aTxtFile.Add(SectionBegin + CurrSection.SectionName + SectionEnd);

      aTxtFile.AddStrings(CurrSection.Lines);

      aTxtFile.Add('');

      Inc(i);
    end;

  finally
    aTxtFile.EndUpdate;
  end;
end;

procedure cEmutecaTagsFile.SectionNameList(aStrList: TStrings);
var
  i: integer;
begin
  if not assigned(aStrList) then
    Exit;
  aStrList.BeginUpdate;
  try
    aStrList.Clear;
    i := 1; // Don't return first empty name section
    while i < Sections.Count do
    begin
      aStrList.Add(Sections[i].SectionName);
      Inc(i);
    end;
  finally
    aStrList.EndUpdate;
  end;
end;

function cEmutecaTagsFile.SectionByName(aSectionName: string):
cEmutecaTagsFileSection;
var
  i: integer;
  aSection: cEmutecaTagsFileSection;
begin
  Result := nil;
  i := 0;
  while i < Sections.Count do
  begin
    aSection := Sections[i];

    if UTF8CompareText(aSection.SectionName, aSectionName) = 0 then
    begin
      Result := aSection;
      break;
    end;
    Inc(i);
  end;
end;

procedure cEmutecaTagsFile.AddGroup(const aSectionName: string;
  const aGroupID: string);
var
  aSection: cEmutecaTagsFileSection;
begin
  if aGroupID = '' then Exit;

  aSection := SectionByName(aSectionName);

  if not assigned(aSection) then
  begin
    aSection := cEmutecaTagsFileSection.Create(self);
    aSection.SectionName := aSectionName;
    Sections.Add(aSection);
  end;

  aSection.Lines.Add(aGroupID);
end;

procedure cEmutecaTagsFile.RemoveGroup(const aSectionName: string;
  const aGroupID: string);
var
  aSection: cEmutecaTagsFileSection;
  aPos: integer;
begin
  if aGroupID = '' then Exit;

  aSection := SectionByName(aSectionName);

  if not assigned(aSection) then Exit;

  if aSection.Lines.Find(aGroupID, aPos) then aSection.Lines.Delete(aPos);

  // Deleting section if it's empty
  if aSection.Lines.Count = 0 then Sections.Remove(aSection);
end;

procedure cEmutecaTagsFile.ANDTagsFile(aFilename: string);
var
  TagsFile: TStringList;
begin
  if not FileExistsUTF8(aFilename) then
  begin
    Sections.Clear;
    Exit;
  end;

  TagsFile := TStringList.Create;
  TagsFile.LoadFromFile(aFilename);
  try
    ANDTagsFile(TagsFile);
  finally
    TagsFile.Free;
  end;
end;

procedure cEmutecaTagsFile.ANDTagsFile(aStrList: TStrings);
var
  ANDTags: cEmutecaTagsFile;
  CurrSection, ANDSection: cEmutecaTagsFileSection;
  i, j: integer;
begin
  ANDTags := cEmutecaTagsFile.Create(nil);
  try
    ANDTags.LoadFromStrLst(aStrList);

    i := 0;
    while i < Sections.Count do
    begin
      CurrSection := Sections[i];

      if (i <> 0) or (CurrSection.SectionName <> '') then
      begin
        ANDSection := ANDTags.SectionByName(CurrSection.SectionName);

        if not assigned(ANDSection) then
        begin
          // Section don't exists in both files, delete it
          Sections.Delete(i);
        end
        else
        begin
          CurrSection.Lines.BeginUpdate;
          ANDSection.Lines.BeginUpdate;
          try
            j := 0;
            while (j < CurrSection.Lines.Count) do
            begin

              while (ANDSection.Lines.Count > 0) and
                (UTF8CompareText(CurrSection.Lines[j], ANDSection.Lines[0]) > 0) do
                ANDSection.Lines.Delete(0);

              if ANDSection.Lines.Count <= 0 then
              begin
                // ANDSection finished, removing remaining CurrSection lines
                while (j < CurrSection.Lines.Count) do
                  CurrSection.Lines.Delete(j);
              end
              else
              begin
                if UTF8CompareText(CurrSection.Lines[j],
                  ANDSection.Lines[0]) = 0 then
                  Inc(j)
                else
                  CurrSection.Lines.Delete(j);
              end;
            end;
          finally
            ANDSection.Lines.EndUpdate;
            CurrSection.Lines.EndUpdate;
          end;

          Inc(i);
        end;
      end
      else
        Inc(i);
    end;

  finally
    ANDTags.Free;
  end;
end;

{ cEmutecaTagsFileSection }

procedure cEmutecaTagsFileSection.SetSectionName(const AValue: string);
begin
  if FSectionName = AValue then
    Exit;
  FSectionName := AValue;
end;

constructor cEmutecaTagsFileSection.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FLines := TStringList.Create;
  Lines.CaseSensitive := False;
  Lines.Sorted := True;
  Lines.Duplicates := dupIgnore;
end;

destructor cEmutecaTagsFileSection.Destroy;
begin
  Lines.Free;
  inherited Destroy;
end;

initialization
  RegisterClass(cEmutecaTagsFileSection);
  RegisterClass(cEmutecaTagsFile);

finalization
  UnRegisterClass(cEmutecaTagsFileSection);
  UnRegisterClass(cEmutecaTagsFile);
end.
