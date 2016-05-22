unit ucEmutecaTagsManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // Emuteca
  ucEmutecaGameTag,
  // Custom
  uGenericGroupManager;

type
  cEmutecaTagsManager = specialize cGenericGroupManager<cEmutecaTag>;

implementation

end.
