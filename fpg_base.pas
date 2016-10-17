{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2013 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      The Big Bang starts here!  The starting unit for fpGUI.
}

unit fpg_base;

{$mode objfpc}{$H+}

// To enable the AggPas powered Canvas
{.$define AGGCanvas}

// For debug use only
{.$define GDEBUG}

interface

uses
  Classes,
  SysUtils,

  syncobjs, // TCriticalSection usage
  variants, contnrs;

type
  TfpgCoord       = integer;     // we might use floating point coordinates in the future...
  TfpgColor       = type longword;    // Always in AARRGGBB (Alpha, Red, Green, Blue) format!!
  TfpgString      = type AnsiString;
  TfpgChar        = type String[4];

  PPoint = ^TPoint;

  TRGBTriple = packed record
    Red: byte;
    Green: byte;
    Blue: byte;
    Alpha: byte;
  end;

  // Same declaration as in FPImage unit, but we don't use FPImage yet, so declare it here
  TFPColor = record
    Red: byte;
    Green: byte;
    Blue: byte;
    Alpha: byte;
  end deprecated;

  TWindowType = (wtChild, wtWindow, wtModalForm, wtPopup);

  TWindowAttribute = (waSizeable, waAutoPos, waScreenCenterPos, waStayOnTop,
      waFullScreen, waBorderless, waUnblockableMessages, waX11SkipWMHints,
      waOneThirdDownPos);
  TWindowAttributes = set of TWindowAttribute;

  TfpgWindowState = (wsNormal, wsMinimized, wsMaximized);

  TMouseCursor = (mcDefault, mcArrow, mcCross, mcIBeam, mcSizeEW, mcSizeNS,
      mcSizeNWSE, mcSizeNESW, mcSizeSWNE, mcSizeSENW, mcMove, mcHourGlass,
      mcHand, mcDrag, mcNoDrop);

  TGradientDirection = (gdVertical,     // Fill vertical
                        gdHorizontal);  // Fill Horizontal

  TClipboardKeyType = (ckNone, ckCopy, ckPaste, ckCut);

  // If you have to convert this to an Integer, mrNone = 0 etc.
  TfpgModalResult = (mrNone, mrOK, mrCancel, mrYes, mrNo, mrAbort, mrRetry,
      mrIgnore, mrAll, mrNoToAll, mrYesToAll, mrHelp);

  TfpgDropAction = (daIgnore, daCopy, daMove, daLink, daAsk);
  TfpgDropActions = set of TfpgDropAction;

  TfpgEditBorderStyle = (ebsNone, ebsDefault, ebsSingle);

  // in case we wanted to trap any fpGUI specific exceptions
  EfpGUIException = class(Exception);

  // For providing user feedback. No need to display backtrace information
  EfpGUIUserFeedbackException = class(EfpGUIException);

  TfpgTextEncoding = (encUTF8, encCP437, encCP850, encCP866, encCP1250, encIBMGraph);



const
  MOUSE_LEFT       = 1;
  MOUSE_RIGHT      = 3;
  MOUSE_MIDDLE     = 2;

  // Platform independent messages used by fpGUI (TfpgWidget)
  FPGM_PAINT       = 1;
  FPGM_ACTIVATE    = 2;
  FPGM_DEACTIVATE  = 3;
  FPGM_KEYPRESS    = 4;
  FPGM_KEYRELEASE  = 5;
  FPGM_KEYCHAR     = 6;
  FPGM_MOUSEDOWN   = 7;
  FPGM_MOUSEUP     = 8;
  FPGM_MOUSEMOVE   = 9;
  FPGM_DOUBLECLICK = 10;
  FPGM_MOUSEENTER  = 11;
  FPGM_MOUSEEXIT   = 12;
  FPGM_CLOSE       = 13;
  FPGM_SCROLL      = 14;
  FPGM_RESIZE      = 15;
  FPGM_MOVE        = 16;
  FPGM_POPUPCLOSE  = 17;
  FPGM_HINTTIMER   = 18;
  FPGM_FREEME      = 19;
  FPGM_DROPENTER   = 20;
  FPGM_DROPEXIT    = 21;
  FPGM_HSCROLL      = 22;
  FPGM_USER        = 50000;
  FPGM_KILLME      = MaxInt;



var
  {$IFDEF MSWINDOWS}
  FPG_DEFAULT_FONT_DESC: string = 'Arial-8:antialias=true';
  FPG_DEFAULT_SANS: string = 'Arial';
  {$ENDIF}
  {$IFDEF UNIX}
  FPG_DEFAULT_FONT_DESC: string = 'Liberation Sans-10:antialias=true';
  FPG_DEFAULT_SANS: string = 'Liberation Sans';
  {$ENDIF}

const
  UserNamedColorStart   = 128;

type
  TfpgRect = object  // not class for static allocations
    Top: TfpgCoord;
    Left: TfpgCoord;
    Width: TfpgCoord;
    Height: TfpgCoord;
    procedure SetRect(aleft, atop, awidth, aheight: TfpgCoord);
    function  Bottom: TfpgCoord;
    function  Right: TfpgCoord;
    procedure SetBottom(Value: TfpgCoord);
    procedure SetRight(Value: TfpgCoord);
  end;


  TfpgPoint = object  // not class for static allocations
    X: integer;
    Y: integer;
    procedure SetPoint(AX, AY: integer);
    function  ManhattanLength: integer;      { See URL for explanation http://en.wikipedia.org/wiki/Taxicab_geometry }
    function  ManhattanLength(const PointB: TfpgPoint): integer;
  end;


  TfpgSize = object  // not class for static allocations
    W: integer;
    H: integer;
    procedure SetSize(AWidth, AHeight: integer);
  end;


  TfpgMsgParmMouse = record
    x: TfpgCoord;
    y: TfpgCoord;
    Buttons: word;
    shiftstate: TShiftState;
    delta: Integer;
    timestamp: TDateTime;  // for future use
  end;


  TfpgMsgParmKeyboard = record
    keycode: word;
    keychar: TfpgChar;
    shiftstate: TShiftState;
  end;


  TfpgMsgParmUser = record
    Param1: Integer;
    Param2: Integer;
    Param3: Integer;
  end;


  TfpgMessageParams = record
    case integer of
      0: (mouse: TfpgMsgParmMouse);
      1: (keyboard: TfpgMsgParmKeyboard);
      2: (rect: TfpgRect);
      3: (user: TfpgMsgParmUser);
  end;


  TfpgMessageRec = record
    MsgCode: integer;
    Sender: TObject;
    Dest: TObject;
    Params: TfpgMessageParams;
    Stop: Boolean;
  end;
  PfpgMessageRec = ^TfpgMessageRec;


  TfpgMoveEventRec = record
    Sender: TObject;
    x: TfpgCoord;
    y: TfpgCoord;
  end;


  TfpgLineStyle = (lsSolid, lsDash, lsDot, lsDashDot, lsDashDotDot);


  // forward declaration
  TfpgWindowBase = class;
  TfpgCanvasBase = class;


  TfpgImageBase = class(TObject)
  private
    function    GetColor(x, y: TfpgCoord): TfpgColor;
    procedure   SetColor(x, y: TfpgCoord; const AValue: TfpgColor);
  protected
    FWidth: integer;
    FHeight: integer;
    FColorDepth: integer;
    FMasked: boolean;
    FImageData: pointer;
    FImageDataSize: integer;
    FMaskData: pointer;
    FMaskDataSize: integer;
    FMaskPoint: TPoint;
    procedure   DoFreeImage; virtual; abstract;
    procedure   DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer); virtual; abstract;
    procedure   DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer); virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Invert(IncludeMask: Boolean = False);
    procedure   FreeImage;
    procedure   AllocateImage(acolordepth, awidth, aheight: integer);
    procedure   AllocateMask;
    procedure   CreateMaskFromSample(x, y: TfpgCoord);
    { Must always be called AFTER you populated the ImageData array. Then only does it allocate OS resources. }
    procedure   UpdateImage;
    { Internal representation of color data is always ARGB }
    property    ImageData: pointer read FImageData;
    property    ImageDataSize: integer read FImageDataSize;
    property    MaskData: pointer read FMaskData;
    property    MaskDataSize: integer read FMaskDataSize;
    property    Width: integer read FWidth;
    property    Height: integer read FHeight;
    property    ColorDepth: integer read FColorDepth;
    property    Masked: boolean read FMasked;
    property    MaskPoint: TPoint read FMaskPoint;
    property    Colors[x, y: TfpgCoord]: TfpgColor read GetColor write SetColor;
  end;


  TfpgFontResourceBase = class(TObject)
  public
    function    GetAscent: integer; virtual; abstract;
    function    GetDescent: integer; virtual; abstract;
    function    GetHeight: integer; virtual; abstract;
    function    GetTextWidth(const txt: string): integer; virtual; abstract;
  end;


  TfpgFontBase = class(TObject)
  protected
    FFontDesc: string;
    FFontRes: TfpgFontResourceBase;
    function    GetIsFixedWidth: boolean; virtual;
  public
    function    TextWidth(const txt: TfpgString): integer;
    function    Ascent: integer;
    function    Descent: integer;
    function    Height: integer;
    property    FontDesc: string read FFontDesc;
    property    FontRes: TfpgFontResourceBase read FFontRes;
    property    Handle: TfpgFontResourceBase read FFontRes;
    property    IsFixedWidth: boolean read GetIsFixedWidth;
  end;


  TfpgCustomInterpolation = class(TObject)
  private
    FCanvas: TfpgCanvasBase;
    FImage: TfpgImageBase;
  protected
    procedure   Initialize(AImage: TfpgImageBase; ACanvas: TfpgCanvasBase); virtual;
    procedure   Execute(x, y, w, h: integer); virtual; abstract;
  public
    property    Canvas: TfpgCanvasBase read FCanvas;
    property    Image: TfpgImageBase read FImage;
  end;


  TfpgBaseInterpolation = class(TfpgCustomInterpolation)
  private
    xfactor: double;
    yfactor: double;
    xsupport: double;
    ysupport: double;
    tempimage: TfpgImageBase;
    procedure   Horizontal(width: integer);
    procedure   Vertical(dx, dy, width, height: integer);
  protected
    procedure   Execute(x, y, w, h: integer); override;
    function    Filter(x : double): double; virtual; abstract;
    function    MaxSupport: double; virtual; abstract;
  public
    destructor  Destroy; override;
  end;


  TfpgMitchelInterpolation = class(TfpgBaseInterpolation)
  protected
    function    Filter(x: double): double; override;
    function    MaxSupport: double; override;
  end;


  TfpgCanvasBase = class(TObject)
  private
    FFastDoubleBuffer: Boolean;
    FInterpolation: TfpgCustomInterpolation;
    procedure SetInterpolation(const AValue: TfpgCustomInterpolation);
  protected
    FBufferedDraw: boolean;
    FBeginDrawCount: integer;
    FWindow: TfpgWindowBase;
    FColor: TfpgColor;
    FTextColor: TfpgColor;
    FLineWidth: integer;
    FLineStyle: TfpgLineStyle;
    FFont: TfpgFontBase;
    FPersistentResources: boolean;
    procedure   DoSetFontRes(fntres: TfpgFontResourceBase); virtual; abstract;
    procedure   DoSetTextColor(cl: TfpgColor); virtual; abstract;
    procedure   DoSetColor(cl: TfpgColor); virtual; abstract;
    procedure   DoSetLineStyle(awidth: integer; astyle: TfpgLineStyle); virtual; abstract;
    procedure   DoGetWinRect(out r: TfpgRect); virtual; abstract;
    procedure   DoFillRectangle(x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoXORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoFillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord); virtual; abstract;
    procedure   DoDrawRectangle(x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoDrawLine(x1, y1, x2, y2: TfpgCoord); virtual; abstract;
    procedure   DoDrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer); virtual; abstract;
    procedure   DoDrawString(x, y: TfpgCoord; const txt: string); virtual; abstract;
    procedure   DoSetClipRect(const ARect: TfpgRect); virtual; abstract;
    function    DoGetClipRect: TfpgRect; virtual; abstract;
    procedure   DoAddClipRect(const ARect: TfpgRect); virtual; abstract;
    procedure   DoClearClipRect; virtual; abstract;
    procedure   DoBeginDraw(awin: TfpgWindowBase; buffered: boolean); virtual; abstract;
    procedure   DoPutBufferToScreen(x, y, w, h: TfpgCoord); virtual; abstract;
    procedure   DoEndDraw; virtual; abstract;
    function    GetPixel(X, Y: integer): TfpgColor; virtual; abstract;
    procedure   SetPixel(X, Y: integer; const AValue: TfpgColor); virtual; abstract;
    procedure   DoDrawArc(x, y, w, h: TfpgCoord; a1, a2: Extended); virtual; abstract;
    procedure   DoFillArc(x, y, w, h: TfpgCoord; a1, a2: Extended); virtual; abstract;
    procedure   DoDrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean = False); virtual; abstract;
  public
    constructor Create(awin: TfpgWindowBase); virtual;
    destructor  Destroy; override;
    procedure   DrawRectangle(x, y, w, h: TfpgCoord); overload;
    procedure   DrawRectangle(r: TfpgRect); overload;
    procedure   DrawLine(x1, y1, x2, y2: TfpgCoord);
    procedure   DrawLineClipped(var x1, y1, x2, y2: TfpgCoord; const AClipRect: TfpgRect);
    procedure   ClipLine(var x1, y1, x2, y2: TfpgCoord; const AClipRect: TfpgRect; out FallsOutsideRegion: Boolean);
    procedure   DrawImage(x, y: TfpgCoord; img: TfpgImageBase);
    procedure   DrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi, yi, w, h: integer);
    procedure   DrawArc(x, y, w, h: TfpgCoord; a1, a2: double);
    procedure   DrawPolygon(const Points: array of TPoint; Winding: Boolean; StartIndex: Integer = 0; NumPts: Integer = -1);
    procedure   DrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean = False); virtual;
    procedure   DrawPolygon(const Points: array of TPoint);
    procedure   StretchDraw (x, y, w, h: TfpgCoord; ASource: TfpgImageBase);
    procedure   CopyRect(ADest_x, ADest_y: TfpgCoord; ASrcCanvas: TfpgCanvasBase; var ASrcRect: TfpgRect); virtual;
    // x,y is the top/left corner of where the text output will start.
    procedure   DrawString(x, y: TfpgCoord; const txt: string);
    procedure   FillRectangle(x, y, w, h: TfpgCoord); overload;
    procedure   FillRectangle(r: TfpgRect); overload;
    procedure   FillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
    procedure   FillArc(x, y, w, h: TfpgCoord; a1, a2: double);
    procedure   GradientFill(ARect: TfpgRect; AStart, AStop: TfpgColor; ADirection: TGradientDirection);
    procedure   XORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord); overload;
    procedure   XORFillRectangle(col: TfpgColor; r: TfpgRect); overload;
    procedure   SetClipRect(const ARect: TfpgRect);
    function    GetClipRect: TfpgRect;
    function    GetLineWidth: integer;
    procedure   AddClipRect(const ARect: TfpgRect);
    procedure   ClearClipRect;
    procedure   Clear(AColor: TfpgColor);
    procedure   GetWinRect(out r: TfpgRect);
    procedure   SetColor(AColor: TfpgColor);
    procedure   SetTextColor(AColor: TfpgColor);
    procedure   SetLineStyle(AWidth: integer; AStyle: TfpgLineStyle);
    procedure   SetFont(AFont: TfpgFontBase);
    procedure   BeginDraw; overload;
    procedure   BeginDraw(ABuffered: boolean); overload;
    procedure   EndDraw(x, y, w, h: TfpgCoord); overload;
    procedure   EndDraw(ARect: TfpgRect); overload;
    procedure   EndDraw; overload;
    procedure   FreeResources;
    property    Color: TfpgColor read FColor write SetColor;
    property    TextColor: TfpgColor read FTextColor write SetTextColor;
    property    Font: TfpgFontBase read FFont write SetFont;
    property    Pixels[X, Y: integer]: TfpgColor read GetPixel write SetPixel;
    property    InterpolationFilter: TfpgCustomInterpolation read FInterpolation write SetInterpolation;
    property    FastDoubleBuffer: Boolean read FFastDoubleBuffer write FFastDoubleBuffer;
    property    LineStyle: TfpgLineStyle read FLineStyle;
  end;

  TfpgCanvasBaseClass = class of TfpgCanvasBase;


  TfpgComponent = class(TComponent)
  private
    FTagPointer: Pointer;
    FHelpContext: THelpContext;
    FHelpKeyword: TfpgString;
    FHelpType: THelpType;
  protected
    procedure   SetHelpContext(const AValue: THelpContext); virtual;
    procedure   SetHelpKeyword(const AValue: TfpgString); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property    TagPointer: Pointer read FTagPointer write FTagPointer;
  published
    property    HelpContext: THelpContext read FHelpContext write SetHelpContext default 0;
    property    HelpKeyword: TfpgString read FHelpKeyword write SetHelpKeyword;
    property    HelpType: THelpType read FHelpType write FHelpType default htKeyword;
  end;


  TfpgWindowBase = class(TfpgComponent)
  private
    FParent: TfpgWindowBase;
    procedure   SetMouseCursor(const AValue: TMouseCursor);
    function    ConstraintWidth(NewWidth: TfpgCoord): TfpgCoord;
    function    ConstraintHeight(NewHeight: TfpgCoord): TfpgCoord;
  protected
    FMouseCursor: TMouseCursor;
    FWindowType: TWindowType;
    FWindowAttributes: TWindowAttributes;
    FTop: TfpgCoord;
    FLeft: TfpgCoord;
    FWidth: TfpgCoord;
    FHeight: TfpgCoord;
    FPrevTop: TfpgCoord;
    FPrevLeft: TfpgCoord;
    FPrevWidth: TfpgCoord;
    FPrevHeight: TfpgCoord;
    FMinWidth: TfpgCoord;
    FMinHeight: TfpgCoord;
    FMaxHeight: TfpgCoord;
    FMaxWidth: TfpgCoord;
    FCanvas: TfpgCanvasBase;
    FSizeIsDirty: Boolean;
    FPosIsDirty: Boolean;
    FMouseCursorIsDirty: Boolean;
    FOnDragStartDetected: TNotifyEvent;
    FDragActive: boolean;
    FWindowState: TfpgWindowState;
    function    HandleIsValid: boolean; virtual; abstract;
    procedure   DoUpdateWindowPosition; virtual; abstract;
    procedure   DoAllocateWindowHandle(AParent: TfpgWindowBase); virtual; abstract;
    procedure   DoReleaseWindowHandle; virtual; abstract;
    procedure   DoRemoveWindowLookup; virtual; abstract;
    procedure   DoSetWindowVisible(const AValue: Boolean); virtual; abstract;
    procedure   DoMoveWindow(const x: TfpgCoord; const y: TfpgCoord); virtual; abstract;
    function    DoWindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint; virtual; abstract;
    procedure   DoSetWindowTitle(const ATitle: string); virtual; abstract;
    procedure   DoSetMouseCursor; virtual; abstract;
    procedure   DoDNDEnabled(const AValue: boolean); virtual; abstract;
    procedure   DoAcceptDrops(const AValue: boolean); virtual; abstract;
    function    GetWindowState: TfpgWindowState; virtual;
    procedure   SetWindowState(const AValue: TfpgWindowState); virtual;
    procedure   DoDragStartDetected; virtual;
    procedure   SetParent(const AValue: TfpgWindowBase); virtual;
    function    GetParent: TfpgWindowBase; virtual;
    function    GetCanvas: TfpgCanvasBase; virtual;
    procedure   AllocateWindowHandle;
    procedure   ReleaseWindowHandle;
    procedure   SetWindowTitle(const ATitle: string); virtual;
    procedure   SetTop(const AValue: TfpgCoord);
    procedure   SetLeft(const AValue: TfpgCoord);
    procedure   SetHeight(const AValue: TfpgCoord);
    procedure   SetWidth(const AValue: TfpgCoord);
    procedure   HandleMove(x, y: TfpgCoord); virtual;
    procedure   HandleResize(AWidth, AHeight: TfpgCoord); virtual;
    property    OnDragStartDetected: TNotifyEvent read FOnDragStartDetected write FOnDragStartDetected;
    property    WindowState: TfpgWindowState read GetWindowState {write SetWindowState} default wsNormal;
  public
    // The standard constructor.
    constructor Create(AOwner: TComponent); override;
    procedure   AfterConstruction; override;
    // Make some setup before the window shows. Forms modify the window creation parameters.
    procedure   AdjustWindowStyle; virtual;
    // Make some setup before the window shows. Invoked after the window is created.
    procedure   SetWindowParameters; virtual;
    // general properties and functions
    function    Right: TfpgCoord;
    function    Bottom: TfpgCoord;
    procedure   UpdateWindowPosition;
    procedure   MoveWindow(const x: TfpgCoord; const y: TfpgCoord);
    function    WindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint;
    function    HasParent: Boolean; override;
    function    GetClientRect: TfpgRect; virtual;
    function    GetBoundsRect: TfpgRect; virtual;
    procedure   ActivateWindow; virtual; abstract;
    procedure   CaptureMouse; virtual; abstract;
    procedure   ReleaseMouse; virtual; abstract;
    procedure   BringToFront; virtual; abstract;
    procedure   SetFullscreen(AValue: Boolean); virtual;
    property    HasHandle: boolean read HandleIsValid;
    property    WindowType: TWindowType read FWindowType write FWindowType;
    property    WindowAttributes: TWindowAttributes read FWindowAttributes write FWindowAttributes;
    property    Left: TfpgCoord read FLeft write SetLeft;
    property    Top: TfpgCoord read FTop write SetTop;
    property    Width: TfpgCoord read FWidth write SetWidth;
    property    Height: TfpgCoord read FHeight write SetHeight;
    property    MinWidth: TfpgCoord read FMinWidth write FMinWidth;
    property    MinHeight: TfpgCoord read FMinHeight write FMinHeight;
    property    MaxWidth: TfpgCoord read FMaxWidth write FMaxWidth default 0;
    property    MaxHeight: TfpgCoord read FMaxHeight write FMaxHeight default 0;
    property    Canvas: TfpgCanvasBase read GetCanvas;
    property    Parent: TfpgWindowBase read GetParent write SetParent;
    property    MouseCursor: TMouseCursor read FMouseCursor write SetMouseCursor;
  end;


  TfpgApplicationBase = class(TfpgComponent)
  private
    FMainForm: TfpgWindowBase;
    FTerminated: boolean;
    FCritSect: TCriticalSection;
    FHelpKey: word;
    FHelpFile: TfpgString;
    function    GetForm(Index: Integer): TfpgWindowBase;
    function    GetFormCount: integer;
    function    GetTopModalForm: TfpgWindowBase;
    function    GetHelpFile: TfpgString;
  protected
    FOnIdle: TNotifyEvent;
    FIsInitialized: Boolean;
    FModalFormStack: TList;
    function    DoGetFontFaceList: TStringList; virtual; abstract;
    procedure   DoWaitWindowMessage(atimeoutms: integer); virtual; abstract;
    function    MessagesPending: boolean; virtual; abstract;
    function    GetHelpViewer: TfpgString; virtual;
  public
    constructor Create(const AParams: string); virtual; reintroduce;
    destructor  Destroy; override;
    function    GetFontFaceList: TStringList;
    procedure   PushModalForm(AForm: TfpgWindowBase);
    procedure   PopModalForm;
    function    PrevModalForm: TfpgWindowBase;
    function    RemoveWindowFromModalStack(AForm: TfpgWindowBase): Integer;
    procedure   CreateForm(InstanceClass: TComponentClass; out Reference);
    function    GetScreenWidth: TfpgCoord; virtual; abstract;
    function    GetScreenHeight: TfpgCoord; virtual; abstract;
    function    Screen_dpi_x: integer; virtual; abstract;
    function    Screen_dpi_y: integer; virtual; abstract;
    function    Screen_dpi: integer; virtual; abstract;
    procedure   Terminate;
    procedure   Lock;
    procedure   Unlock;
    procedure   InvokeHelp;
    function    ContextHelp(const AHelpContext: THelpContext): Boolean;
    function    KeywordHelp(const AHelpKeyword: string): Boolean;
    property    FormCount: integer read GetFormCount;
    property    Forms[Index: Integer]: TfpgWindowBase read GetForm;
    property    HelpContext;
    property    HelpFile: TfpgString read GetHelpFile write FHelpFile;
    property    IsInitialized: boolean read FIsInitialized;
    property    TopModalForm: TfpgWindowBase read GetTopModalForm;
    property    MainForm: TfpgWindowBase read FMainForm write FMainForm;
    property    Terminated: boolean read FTerminated write FTerminated;
    property    OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
  end;


  TfpgClipboardBase = class(TObject)
  protected
    function    DoGetText: TfpgString; virtual; abstract;
    procedure   DoSetText(const AValue: TfpgString); virtual; abstract;
    procedure   InitClipboard; virtual; abstract;
  public
    constructor Create; virtual;
    property    Text: TfpgString read DoGetText write DoSetText;
  end;


  TFileEntryType = (etFile, etDir);
  TFileListSortOrder = (soNone, soFileName, soCSFileName, soFileExt, soSize, soTime);
  TFileModeString = string[9];
  TfpgSearchMode = (smAny, smFiles, smDirs);


  // A simple data object
  TFileEntry = class(TObject)
  private
    FEntryType: TFileEntryType;
    FExtension: string;
    FName: string;
    FModTime: TDateTime;
    FSize: int64;
    FIsLink: boolean;
    FLinkTarget: string;
    FIsExecutable: boolean;
    FModeString: TFileModeString;
    FOwner: TfpgString;
    FGroup: TfpgString;
    FAttrString: TFileModeString;
  public
    constructor Create;
    property    Name: string read FName write FName;
    property    Extension: string read FExtension write FExtension;
    property    Size: int64 read FSize write FSize;
    property    EntryType: TFileEntryType read FEntryType write FEntryType;
    property    IsLink: boolean read FIsLink write FIsLink;
    property    LinkTarget: string read FLinkTarget write FLinkTarget;
    property    IsExecutable: boolean read FIsExecutable write FIsExecutable;
    property    ModTime: TDateTime read FModTime write FModTime;
    property    Mode: TFileModeString read FModeString write FModeString;
    property    Owner: TfpgString read FOwner write FOwner;
    property    Group: TfpgString read FGroup write FGroup;
    property    Attributes: TFileModeString read FAttrString write FAttrString;
  end;


  TfpgFileListBase = class(TObject)
  private
    FEntries: TList;
    FDirectoryName: TfpgString;
    FFileMask: TfpgString;
    FSearchMode: TfpgSearchMode;
    FShowHidden: boolean;
    FCurrentSpecialDir: integer;
    procedure   AddEntry(sr: TSearchRec);
    function    GetEntry(i: integer): TFileEntry;
    function    HasAttrib(fileAttrib, testAttrib: Integer): Boolean;
  protected
    FSpecialDirs: TStringList;
    FHasFileMode: boolean;
    function    InitializeEntry(sr: TSearchRec): TFileEntry; virtual;
    procedure   PopulateSpecialDirs(const aDirectory: TfpgString); virtual;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    function    Count: integer;
    function    CurrentSpecialDir: integer;
    function    ReadDirectory(const aDirectory: TfpgString = ''): boolean;
    procedure   Clear;
    procedure   Sort(AOrder: TFileListSortOrder);
    property    DirectoryName: TfpgString read FDirectoryName;
    property    Entry[i: integer]: TFileEntry read GetEntry;
    property    FileMask: TfpgString read FFileMask write FFileMask;
    property    HasFileMode: boolean read FHasFileMode;
    property    SearchMode: TfpgSearchMode read FSearchMode write FSearchMode;
    property    ShowHidden: boolean read FShowHidden write FShowHidden;
    property    SpecialDirs: TStringList read FSpecialDirs;
  end;


  TfpgMimeDataItem = class(TObject)
  public
    format: TfpgString;   { mime string type }
    data: Variant;
    constructor Create(const AFormat: TfpgString; const AData: variant); reintroduce;
  end;


  TfpgMimeDataBase = class(TObject)
  private
    { TODO: This is wrong, we must have one Data Storage object }
    FDataList: TObjectList;
    FUrlList: TList;
    function    GetItem(AIndex: Integer): TfpgMimeDataItem;
    function    Geturls: TList;
    procedure   Seturls(const AValue: TList);
    function    GetText: TfpgString;
    procedure   SetText(const AValue: TfpgString);
    function    GetHTML: TfpgString;
    procedure   SetHTML(const AValue: TfpgString);
    function    GetCount: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear;
    function    HasFormat(const AMimeType: TfpgString): boolean;
    function    Formats: TStrings;
    function    GetData(const AMimeType: TfpgString): Variant;
    procedure   SetData(const AMimeType: TfpgString; const AData: Variant);
    property    Items[AIndex: Integer]: TfpgMimeDataItem read GetItem; default;
    property    urls: TList read Geturls write Seturls;
    property    Text: TfpgString read GetText write SetText;
    property    HTML: TfpgString read GetHTML write SetHTML;
    property    Count: integer read GetCount;
  end;


  TfpgDragBase = class(TObject)
  protected
    FDragging: Boolean;
    FMimeData: TfpgMimeDataBase;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Execute(const ADropActions: TfpgDropActions; const ADefaultAction: TfpgDropAction = daCopy): TfpgDropAction; virtual; abstract;
  end;
  
  
  { TfpgBaseTimer }

  TfpgBaseTimer = class(TObject)
  private
    FNextAlarm: TDateTime;
    FInterval: integer;
    FOnTimer: TNotifyEvent;
    procedure   SetInterval(const AValue: integer);
  protected
    FEnabled: boolean;
    procedure   SetEnabled(const AValue: boolean); virtual;
  public
    constructor Create(AInterval: integer); virtual;
    destructor  Destroy; override;
    procedure   CheckAlarm(ACurrentTime: TDateTime);
    procedure   Reset;
    procedure   Pause(ASeconds: integer);
    property    Enabled: boolean read FEnabled write SetEnabled;
    property    NextAlarm: TDateTime read FNextAlarm;
    { Interval is in milliseconds. }
    property    Interval: integer read FInterval write SetInterval;
    property    OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;




{ ********  Helper functions  ******** }
{ Keyboard }
function  KeycodeToText(AKey: Word; AShiftState: TShiftState): string;
function  CheckClipboardKey(AKey: Word;  AShiftstate: TShiftState): TClipboardKeyType;

{ Color }
function  fpgColorToRGBTriple(const AColor: TfpgColor): TRGBTriple;
function  fpgColorToFPColor(const AColor: TfpgColor): TFPColor; deprecated;
function  RGBTripleTofpgColor(const AColor: TRGBTriple): TfpgColor;
function  FPColorTofpgColor(const AColor: TFPColor): TfpgColor; deprecated;
function  fpgGetRed(const AColor: TfpgColor): byte;
function  fpgGetGreen(const AColor: TfpgColor): byte;
function  fpgGetBlue(const AColor: TfpgColor): byte;
function  fpgGetAlpha(const AColor: TfpgColor): byte;
function  fpgGetAvgColor(const AColor1, AColor2: TfpgColor): TfpgColor;
function  fpgColor(const ARed, AGreen, ABlue: byte): TfpgColor;
function  fpgColor(const ARed, AGreen, ABlue, AAlpha: byte): TfpgColor;
function  fpgDarker(const AColor: TfpgColor; APercent: Byte = 50): TfpgColor;
function  fpgLighter(const AColor: TfpgColor; APercent: Byte = 50): TfpgColor;


{ Points }
procedure SortRect(var ARect: TRect);
procedure SortRect(var ARect: TfpgRect);
procedure SortRect(var left, top, right, bottom: integer);



implementation

uses
  typinfo,
  process,
  {$IFDEF GDEBUG}
  fpg_dbugintf,
  {$ENDIF}
  dateutils;


const
  NoDefault = $80000000;
  tkPropsWithDefault = [tkInteger, tkChar, tkSet, tkEnumeration];


procedure SortRect(var ARect: TRect);
begin
  with ARect do
    SortRect(left, top, right, bottom);
end;

procedure SortRect(var ARect: TfpgRect);
var
  r: TfpgCoord;
  b: TfpgCoord;
begin
  r := ARect.Right;
  b := ARect.Bottom;
  SortRect(ARect.Left, ARect.Top, r, b);
  ARect.SetRight(r);
  ARect.SetBottom(b);
end;

procedure SortRect(var left, top, right, bottom: integer);
var
  r: integer;
begin
  if left > right then
  begin
    r       := left;
    left    := right;
    right   := r;
  end;
  if top > bottom then
  begin
    r       := top;
    top     := bottom;
    bottom  := r;
  end;
end;

// This function uses RTTI to automatically set the default values of properties.
// That means we don't have to do it in the constructor anymore! :-)
procedure SetDefaults(Obj: TObject);
var
  PropInfos: PPropList;
  Count, Loop: Integer;
begin
  PropInfos := nil;
  { Find out how many properties we'll be considering }
  Count := GetPropList(Obj.ClassInfo, tkPropsWithDefault, nil);
  { Allocate memory to hold their RTTI data }
  GetMem(PropInfos, Count * SizeOf(PPropInfo));
  try
    { Get hold of the property list in our new buffer }
    GetPropList(Obj.ClassInfo, tkPropsWithDefault, PropInfos);
    { Loop through all the selected properties }
    for Loop := 0 to Count - 1 do
    begin
      with PropInfos^[Loop]^ do
      begin
        { If there is supposed to be a default value... }
        if Default <> NoDefault then
          { ...then jolly well set it }
          SetOrdProp(Obj, PropInfos^[Loop], Default)
      end;
    end;
  finally
    FreeMem(PropInfos, Count * SizeOf(PPropInfo));
  end;
end;

{ TfpgRect }

procedure TfpgRect.SetRect(aleft, atop, awidth, aheight: TfpgCoord);
begin
  Left   := aleft;
  Top    := atop;
  Width  := awidth;
  Height := aheight;
end;

function TfpgRect.Bottom: TfpgCoord;
begin
  Result := Top + Height - 1;
end;

function TfpgRect.Right: TfpgCoord;
begin
  Result := Left + Width - 1;
end;

procedure TfpgRect.SetBottom(Value: TfpgCoord);
begin
  Height := Value - Top + 1;
end;

procedure TfpgRect.SetRight(Value: TfpgCoord);
begin
  Width := Value - Left + 1;
end;


{ TfpgPoint }

procedure TfpgPoint.SetPoint(AX, AY: integer);
begin
  X := AX;
  Y := AY;
end;

function TfpgPoint.ManhattanLength: integer;
begin
  Result := Abs(X) + Abs(Y);
end;

function TfpgPoint.ManhattanLength(const PointB: TfpgPoint): integer;
begin
  Result := Abs(PointB.X-X) + Abs(PointB.Y-Y);
end;


{ TfpgSize }

procedure TfpgSize.SetSize(AWidth, AHeight: integer);
begin
  W := AWidth;
  H := AHeight;
end;


{ TfpgWindowBase }

procedure TfpgWindowBase.SetMouseCursor(const AValue: TMouseCursor);
begin
  if FMouseCursor = AValue then
    Exit; //==>
  FMouseCursor := AValue;
  DoSetMouseCursor;
end;

function TfpgWindowBase.ConstraintWidth(NewWidth: TfpgCoord): TfpgCoord;
begin
  Result := NewWidth;
  if (MaxWidth >= MinWidth) and (Result > MaxWidth) and (MaxWidth > 0) then
    Result := MaxWidth;
  if Result < MinWidth then
    Result := MinWidth;
end;

function TfpgWindowBase.ConstraintHeight(NewHeight: TfpgCoord): TfpgCoord;
begin
  Result := NewHeight;
  if (MaxHeight >= MinHeight) and (Result > MaxHeight) and (MaxHeight > 0) then
    Result := MaxHeight;
  if Result < MinHeight then
    Result := MinHeight;
end;

function TfpgWindowBase.GetWindowState: TfpgWindowState;
begin
  Result := FWindowState;
end;

procedure TfpgWindowBase.SetWindowState(const AValue: TfpgWindowState);
begin
  // do nothing
end;

procedure TfpgWindowBase.DoDragStartDetected;
begin
  if Assigned(FOnDragStartDetected) then
    FOnDragStartDetected(self);
end;

procedure TfpgWindowBase.SetParent(const AValue: TfpgWindowBase);
begin
  FParent := AValue;
end;

function TfpgWindowBase.GetParent: TfpgWindowBase;
begin
  result := FParent;
end;

function TfpgWindowBase.GetCanvas: TfpgCanvasBase;
begin
  Result := FCanvas;
end;

procedure TfpgWindowBase.AllocateWindowHandle;
begin
  DoAllocateWindowHandle(FParent);
  if FMouseCursorIsDirty then
    DoSetMouseCursor;
end;

procedure TfpgWindowBase.ReleaseWindowHandle;
begin
  if HasHandle then
  begin
    Canvas.FreeResources;
    DoReleaseWindowHandle;
  end;
  DoRemoveWindowLookup;
end;

procedure TfpgWindowBase.SetWindowTitle(const ATitle: string);
begin
  DoSetWindowTitle(ATitle);
end;

procedure TfpgWindowBase.SetTop(const AValue: TfpgCoord);
begin
  HandleMove(Left, AValue);
end;

procedure TfpgWindowBase.SetLeft(const AValue: TfpgCoord);
begin
  HandleMove(AValue, Top);
end;

procedure TfpgWindowBase.SetHeight(const AValue: TfpgCoord);
begin
  HandleResize(Width, AValue);
end;

procedure TfpgWindowBase.SetWidth(const AValue: TfpgCoord);
begin
  HandleResize(AValue, Height);
end;

procedure TfpgWindowBase.HandleMove(x, y: TfpgCoord);
begin
  if FTop <> y then
  begin
    if not (csLoading in ComponentState) then
      FPrevTop := FTop
    else
      FPrevTop := y;
    FTop := y;
    FPosIsDirty := FPosIsDirty or (FTop <> FPrevTop);
  end;

  if FLeft <> x then
  begin
    if not (csLoading in ComponentState) then
      FPrevLeft := FLeft
    else
      FPrevLeft := x;
    FLeft := x;
    FPosIsDirty := FPosIsDirty or (FLeft <> FPrevLeft);
  end;
end;

procedure TfpgWindowBase.HandleResize(AWidth, AHeight: TfpgCoord);
begin
  if FWidth <> AWidth then
  begin
    if not (csLoading in ComponentState) then
      FPrevWidth := FWidth
    else
      FPrevWidth := AWidth;
    FWidth := ConstraintWidth(AWidth);
    FSizeIsDirty := FSizeIsDirty or (FWidth <> FPrevWidth);
  end;

  if FHeight <> AHeight then
  begin
    if not (csLoading in ComponentState) then
      FPrevHeight := FHeight
    else
      FPrevHeight := AHeight;
    FHeight := ConstraintHeight(AHeight);
    FSizeIsDirty := FSizeIsDirty or (FHeight <> FPrevHeight);
  end;
end;

constructor TfpgWindowBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMouseCursor := mcDefault;
  FMouseCursorIsDirty := False;
  FPosIsDirty := True;
  FSizeIsDirty := True;
  FMaxWidth := 0;
  FMaxHeight := 0;
  FDragActive := False;
  FWindowState := wsNormal;
end;

procedure TfpgWindowBase.AfterConstruction;
begin
  inherited AfterConstruction;
  { There is a neater way by using RTTI to set default property values all
    automatically. No need to duplicate the efforts and manually set the
    property default values in the constructor. This code is now the same for
    each TfpgWindowBase descendant (which includes GUI widgets) }
//  SetDefaults(self);
end;

procedure TfpgWindowBase.AdjustWindowStyle;
begin
  // does nothing here
end;

procedure TfpgWindowBase.SetWindowParameters;
begin
  // does nothing
end;

function TfpgWindowBase.Right: TfpgCoord;
begin
  Result := FLeft + FWidth - 1;
end;

function TfpgWindowBase.Bottom: TfpgCoord;
begin
  Result := FTop + FHeight - 1;
end;

procedure TfpgWindowBase.UpdateWindowPosition;
begin
  DoUpdateWindowPosition;
end;

procedure TfpgWindowBase.MoveWindow(const x: TfpgCoord; const y: TfpgCoord);
begin
  Left  := x;
  Top   := y;
  DoMoveWindow(x, y);
end;

function TfpgWindowBase.WindowToScreen(ASource: TfpgWindowBase; const AScreenPos: TPoint): TPoint;
begin
  Result := DoWindowToScreen(ASource, AScreenPos);
end;

function TfpgWindowBase.HasParent: Boolean;
begin
  Result := FParent <> nil;
end;

function TfpgWindowBase.GetClientRect: TfpgRect;
begin
  Result.SetRect(0, 0, Width, Height);
end;

function TfpgWindowBase.GetBoundsRect: TfpgRect;
begin
  Result.SetRect(Left, Top, Width+1, Height+1);
end;

procedure TfpgWindowBase.SetFullscreen(AValue: Boolean);
begin
  if AValue then
    Include(FWindowAttributes, waFullScreen)
  else
    Exclude(FWindowAttributes, waFullScreen);
  // now decendants must override this and implement the actualy fullscreen part
end;

{ TfpgCanvasBase }

procedure TfpgCanvasBase.SetInterpolation(const AValue: TfpgCustomInterpolation);
begin
  FInterpolation.Free;
  FInterpolation := AValue;
end;

constructor TfpgCanvasBase.Create(awin: TfpgWindowBase);
begin
  FBufferedDraw := True;
  FFastDoubleBuffer := True;
  FWindow := awin;
end;

destructor TfpgCanvasBase.Destroy;
begin
  FInterpolation.Free;
  inherited Destroy;
end;

procedure TfpgCanvasBase.DrawRectangle(x, y, w, h: TfpgCoord);
begin
  DoDrawRectangle(x, y, w, h);
end;

procedure TfpgCanvasBase.DrawRectangle(r: TfpgRect);
begin
  DoDrawRectangle(r.Left, r.Top, r.Width, r.Height);
end;

procedure TfpgCanvasBase.DrawLine(x1, y1, x2, y2: TfpgCoord);
begin
  DoDrawLine(x1, y1, x2, y2);
end;

procedure TfpgCanvasBase.DrawLineClipped(var x1, y1, x2, y2: TfpgCoord;
  const AClipRect: TfpgRect);
var
  OutOfRegion: boolean;
begin
  ClipLine(X1, Y1, X2, Y2, AClipRect, OutOfRegion);
  if not OutOfRegion then
    DrawLine(X1, Y1, X2, Y2);                { Draw the new line!            }
end;

{ DrawLineClipped - This procedure clips a line to the AClipRect boundaries and
 then calls the DrawLine procedure with the clipped coordinates.  If the line
 lies completely outside of the clip boundary, then the Line routine is not
 called.  This procedure uses the well known Cohen-Sutherland line clipping
 algorithm to clip each coordinate.

 Use this if you did not what to change the Canvas.ClipRegion for some reason.
 For a detailed explanation see:
   http://www.nondot.org/~sabre/graphpro/line6.html                           }
procedure TfpgCanvasBase.ClipLine(var x1, y1, x2, y2: TfpgCoord;
  const AClipRect: TfpgRect; out FallsOutsideRegion: Boolean);
CONST
  CodeBottom = 1; CodeTop    = 2;             { BitFields for output codes }
  CodeLeft   = 4; CodeRight  = 8;

  FUNCTION CompOutCode(X, Y : INTEGER) : integer;  { Nested function }
  VAR Code : integer;
  BEGIN
    Code := 0;
    IF      Y > AClipRect.Bottom THEN Code := CodeBottom
    ELSE IF Y < AClipRect.Top THEN Code := CodeTop;
    IF      X > AClipRect.Right THEN Code := Code+CodeRight
    ELSE IF X < AClipRect.Left THEN Code := Code+CodeLeft;
    Result := Code;
  END;

VAR
  OutCode0,         { The code of the first endpoint  }
  OutCode1,         { The code of the second endpoint }
  OutCodeOut : integer;
  X, Y : INTEGER;
BEGIN
  FallsOutsideRegion := False;
  OutCode0 := CompOutCode(X1, Y1);            { Compute the original codes   }
  OutCode1 := CompOutCode(X2, Y2);

  WHILE (OutCode0 <> 0) OR (OutCode1 <> 0) DO { While not Trivially Accepted }
  BEGIN
    IF (OutCode0 AND OutCode1) <> 0 THEN      { Trivial Reject }
    begin
      FallsOutsideRegion := True;
      Exit;   //==>
    end
    ELSE
    BEGIN        { Failed both tests, so calculate the line segment to clip }
      IF OutCode0 > 0 THEN
        OutCodeOut := OutCode0    { Clip the first point }
      ELSE
        OutCodeOut := OutCode1;   { Clip the last point  }

      IF (OutCodeOut AND CodeBottom) = CodeBottom THEN
      BEGIN               { Clip the line to the bottom of the viewport     }
        Y := AClipRect.Bottom;
        X := X1+LONGINT(X2-X1)*LONGINT(Y-Y1) DIV (Y2 - Y1);
      END
      ELSE IF (OutCodeOut AND CodeTop) = CodeTop THEN
      BEGIN               { Clip the line to the top of the viewport        }
        Y := AClipRect.Top;
        X := X1+LONGINT(X2-X1)*LONGINT(Y-Y1) DIV (Y2 - Y1);
      END
      ELSE IF (OutCodeOut AND CodeRight) = CodeRight THEN
      BEGIN               { Clip the line to the right edge of the viewport }
        X := AClipRect.Right;
        Y := Y1+LONGINT(Y2-Y1)*LONGINT(X-X1) DIV (X2-X1);
      END
      ELSE IF (OutCodeOut AND CodeLeft) = CodeLeft THEN
      BEGIN               { Clip the line to the left edge of the viewport  }
        X := AClipRect.Left;
        Y := Y1+LONGINT(Y2-Y1)*LONGINT(X-X1) DIV (X2-X1);
      END;

      IF (OutCodeOut = OutCode0) THEN       { Modify the first coordinate   }
      BEGIN
        X1 := X; Y1 := Y;                   { Update temporary variables    }
        OutCode0 := CompOutCode(X1, Y1);    { Recalculate the OutCode       }
      END
      ELSE                                  { Modify the second coordinate  }
      BEGIN
        X2 := X; Y2 := Y;                   { Update temporary variables    }
        OutCode1 := CompOutCode(X2, Y2);    { Recalculate the OutCode       }
      END;
    END;
  END;  { while }
end;

procedure TfpgCanvasBase.DrawImage(x, y: TfpgCoord; img: TfpgImageBase);
begin
  if img = nil then
    Exit; //==>
  DrawImagePart(x, y, img, 0, 0, img.Width, img.Height);
end;

procedure TfpgCanvasBase.DrawImagePart(x, y: TfpgCoord; img: TfpgImageBase; xi,
  yi, w, h: integer);
begin
  DoDrawImagePart(x, y, img, xi, yi, w, h);
end;

procedure TfpgCanvasBase.DrawArc(x, y, w, h: TfpgCoord; a1, a2: double);
begin
  DoDrawArc(x, y, w, h, a1, a2);
end;

{ Use Polygon to draw a closed, many-sided shape on the canvas, using the value
  of Canvas.Color. The shape is always filled.
  The Points parameter is an array of points that give the vertices of the
  polygon.
  Winding determines how the polygon is filled. When Winding is True, Polygon
  fills the shape using the Winding fill algorithm. When Winding is False,
  Polygon uses the even-odd (alternative) fill algorithm.
  StartIndex gives the index of the first point in the array to use. All points
  before this are ignored.
  NumPts indicates the number of points to use, starting at StartIndex.
  If NumPts is -1 (the default), Polygon uses all points from StartIndex to the
  end of the array.
  The first point is always connected to the last point.
  To draw a polygon on the canvas, without filling it, use the Polyline method,
  specifying the first point a second time at the end. }
procedure TfpgCanvasBase.DrawPolygon(const Points: array of TPoint;
  Winding: Boolean; StartIndex: Integer; NumPts: Integer);
var
  NPoints: integer;
begin
  if NumPts<0 then
    NPoints:=High(Points)-StartIndex+1
  else
    NPoints:=NumPts;
  if NPoints<=0 then exit;
  DrawPolygon(@Points[StartIndex],NPoints,Winding);
end;

procedure TfpgCanvasBase.DrawPolygon(Points: PPoint; NumPts: Integer;
  Winding: boolean);
begin
  if NumPts<=0 then exit;
  DoDrawPolygon(Points,NumPts,Winding);
end;

procedure TfpgCanvasBase.DrawPolygon(const Points: array of TPoint);
begin
  DrawPolygon(Points, True, Low(Points), High(Points) - Low(Points) + 1);
end;

procedure TfpgCanvasBase.StretchDraw(x, y, w, h: TfpgCoord; ASource: TfpgImageBase);
var
  FreeInterpolation: boolean;
  IP: TfpgCustomInterpolation;
begin
  FreeInterpolation := not Assigned(FInterpolation);
  if FreeInterpolation then
    IP := TfpgMitchelInterpolation.Create
  else
    IP := FInterpolation;
  try
    IP.Initialize(ASource, self);
    IP.Execute(x, y, w, h);
  finally
    if FreeInterpolation then
      IP.Free;
  end;
end;

procedure TfpgCanvasBase.CopyRect(ADest_x, ADest_y: TfpgCoord; ASrcCanvas: TfpgCanvasBase;
  var ASrcRect: TfpgRect);
var
  x, sx, y, sy: TfpgCoord;
begin
  SortRect(ASrcRect);
  // X position of source
  for sx := ASrcRect.Left to ASrcRect.Right do
  begin
    x := ADest_x + (sx - ASrcRect.Left);  // calc dest x
    // Y position of source
    for sy := ASrcRect.Top to ASrcRect.Bottom do
    begin
      y := ADest_y + (sy - ASrcRect.Top); // calc dest y
      Pixels[x, y] := ASrcCanvas.Pixels[sx, sy];
    end;
  end;
end;

procedure TfpgCanvasBase.DrawString(x, y: TfpgCoord; const txt: string);
var
  underline: integer;
begin
  DoDrawString(x, y, txt);

  { What was not handled: underline }
  if Pos('UNDERLINE', UpperCase(Font.FontDesc)) > 0 then
  begin
    underline := (Font.Descent div 2) + 1;
    if underline = 0 then
      underline := 1;
    if underline >= Font.Descent then
      underline := Font.Descent - 1;

    DoSetLineStyle(1, lsSolid);
    DoSetColor(TextColor);
    DoDrawLine(x, y+Font.Height-underline, x+Font.TextWidth(txt), y+Font.Height-underline);
  end;
end;

procedure TfpgCanvasBase.FillRectangle(x, y, w, h: TfpgCoord);
begin
  DoFillRectangle(x, y, w, h);
end;

procedure TfpgCanvasBase.FillRectangle(r: TfpgRect);
begin
  DoFillRectangle(r.Left, r.Top, r.Width, r.Height);
end;

procedure TfpgCanvasBase.FillTriangle(x1, y1, x2, y2, x3, y3: TfpgCoord);
begin
  DoFillTriangle(x1, y1, x2, y2, x3, y3);
end;

procedure TfpgCanvasBase.FillArc(x, y, w, h: TfpgCoord; a1, a2: double);
begin
  DoFillArc(x, y, w, h, a1, a2);
end;

procedure TfpgCanvasBase.GradientFill(ARect: TfpgRect; AStart, AStop: TfpgColor;
  ADirection: TGradientDirection);
var
  RGBStart: TRGBTriple;
  RGBStop: TRGBTriple;
  RDiff, GDiff, BDiff: Integer;
  count: Integer;
  i: Integer;
  newcolor: TRGBTriple;
begin
  RGBStart := fpgColorToRGBTriple(AStart);
  RGBStop  := fpgColorToRGBTriple(AStop);

  if ADirection = gdVertical then
    count := ARect.Height
  else
    count := ARect.Width;

  RDiff := RGBStop.Red - RGBStart.Red;
  GDiff := RGBStop.Green - RGBStart.Green;
  BDiff := RGBStop.Blue - RGBStart.Blue;

//  Changing;
  for i := 0 to count do
  begin
    newcolor.Red    := RGBStart.Red + (i * RDiff) div count;
    newcolor.Green  := RGBStart.Green + (i * GDiff) div count;
    newcolor.Blue   := RGBStart.Blue + (i * BDiff) div count;
    SetColor(RGBTripleTofpgColor(newcolor));

    // We have to overshoot by 1 pixel as DrawLine paints 1 pixel short (by design)
    if ADirection = gdHorizontal then
      DrawLine(ARect.Left+i, ARect.Top, ARect.Left+i, ARect.Bottom+1)
    else
      DrawLine(ARect.Left, ARect.Top+i, ARect.Right+1, ARect.Top+i);
  end;
//  Changed;
end;

procedure TfpgCanvasBase.XORFillRectangle(col: TfpgColor; x, y, w, h: TfpgCoord);
begin
  DoXORFillRectangle(col, x, y, w, h);
end;

procedure TfpgCanvasBase.XORFillRectangle(col: TfpgColor; r: TfpgRect);
begin
  DoXORFillRectangle(col, r.Left, r.Top, r.Width, r.Height);
end;

procedure TfpgCanvasBase.SetClipRect(const ARect: TfpgRect);
begin
  DoSetClipRect(ARect);
end;

function TfpgCanvasBase.GetClipRect: TfpgRect;
begin
  Result := DoGetClipRect;
end;

function TfpgCanvasBase.GetLineWidth: integer;
begin
  Result := FLineWidth;
end;

procedure TfpgCanvasBase.AddClipRect(const ARect: TfpgRect);
begin
  DoAddClipRect(ARect);
end;

procedure TfpgCanvasBase.ClearClipRect;
begin
  DoClearClipRect;
end;

procedure TfpgCanvasBase.Clear(AColor: TfpgColor);
var
  lCol:     TfpgColor;
  lWinRect: TfpgRect;
begin
  lCol := FColor;
  DoSetColor(AColor);
  DoGetWinRect(lWinRect);
  DoFillRectangle(0, 0, lWinRect.Width, lWinRect.Height);
  DoSetColor(lCol);
end;

procedure TfpgCanvasBase.GetWinRect(out r: TfpgRect);
begin
  DoGetWinRect(r);
end;

procedure TfpgCanvasBase.SetColor(AColor: TfpgColor);
begin
  FColor := AColor;
  DoSetColor(FColor);
end;

procedure TfpgCanvasBase.SetTextColor(AColor: TfpgColor);
begin
  FTextColor := AColor;
  DoSetTextColor(FTextColor);
end;

procedure TfpgCanvasBase.SetLineStyle(AWidth: integer; AStyle: TfpgLineStyle);
begin
  FLineWidth := AWidth;
  FLineStyle := AStyle;
  DoSetLineStyle(FLineWidth, FLineStyle);
end;

procedure TfpgCanvasBase.SetFont(AFont: TfpgFontBase);
begin
  if AFont = nil then
    exit;
  FFont := AFont;
  DoSetFontRes(AFont.FFontRes);
end;

procedure TfpgCanvasBase.BeginDraw;
begin
  BeginDraw(FBufferedDraw);
end;



