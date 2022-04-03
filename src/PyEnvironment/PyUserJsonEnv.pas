unit PyUserJsonEnv;

interface

uses
  PyEnvironment;

type
  (*-----------------------------------------------------------------------*)
  (*                                                                       *)
  (*                         JSON structure example                        *)
  (*                                                                       *)
  (*                                                                       *)
  (*                                                                       *)
  (*  JSON                                                                 *)
  (*    {"platform": [                                                     *)
  (*      {"arch": [                                                       *)
  (*          {"python_version":                                           *)
  (*            {"home": "",                                               *)
  (*             "program_name": "",                                       *)
  (*             "shared_library": "",                                     *)
  (*             "executable": ""}}]}]}                                    *)
  (*-----------------------------------------------------------------------*)
  /// <summary>
  ///   Provide access to the Python environment based on a JSON file.
  ///   Use "any" for global configuration.
  /// </summary>
  TPyUserJsonEnv = class(TPyEnvironment)
  private
    FFilePath: string;
  protected
    /// <summary>
    ///   Having the FilePath empty, the app path is used as root.
    /// </summary>
    function ResolveFilePath(): string;
  published
    /// <summary>
    ///   Specifies the JSON file path. Let it empty for default settings.
    /// </summary>
    property FilePath: string read FFilePath write FFilePath;
  end;

implementation

{ TPyUserJsonEnv }

function TPyUserJsonEnv.ResolveFilePath: string;
begin

end;

end.
