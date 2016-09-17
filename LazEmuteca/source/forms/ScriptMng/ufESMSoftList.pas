unit ufESMSoftList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, VirtualTrees, Forms, Controls, Graphics,
  Dialogs;

type

  { TfrmESMVersionList }

  TfrmESMVersionList = class(TForm)
    vstSoftware: TVirtualStringTree;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmESMVersionList: TfrmESMVersionList;

implementation

{$R *.lfm}

end.

