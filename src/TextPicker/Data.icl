implementation module TextPicker.Data

import StdDebug
import StdEnv

import qualified Control.Monad
import Data.Error
import Data.Error.GenJSON
import Data.Func
from Text import concat5

import iTasks

import TextFabric
import TextFabric.Import

FEATURES :==
	[ "book"
	, "chapter"
	, "gloss"
	, "lex"
	, "pargr"
	, "verse"
	, "voc_lex_utf8"
	, "vs"
	, "vt"

	, "g_word_utf8"
	, "trailer_utf8"

	, "gn"
	, "nu"
	, "prs_gn"
	, "prs_nu"
	, "prs_ps"
	, "ps"
	, "st"
	, "vs"
	, "vt"

	, "pdp"
	]
derive class iTask LexemeInformation

gEq{|DataSet`|} _ _ _ = abort "gEq DataSet`\n"
gText{|DataSet`|} _ _ _ = abort "gText DataSet`\n"
gEditor{|DataSet`|} _ _ _ _ = abort "gEditor DataSet`\n"
JSONEncode{|DataSet`|} _ _ _ = abort "JSONEncode DataSet`\n"
JSONDecode{|DataSet`|} _ _ _ = abort "JSONDecode DataSet`\n"

gEq{|EdgeSet|} _ _ = abort "gEq EdgeSet\n"
gText{|EdgeSet|} _ _ = abort "gText EdgeSet\n"
gEditor{|EdgeSet|} _ = abort "gEditor EdgeSet\n"
JSONEncode{|EdgeSet|} _ _ = abort "JSONEncode EdgeSet\n"
JSONDecode{|EdgeSet|} _ _ = abort "JSONDecode EdgeSet\n"

dataPath :: SimpleSDSLens String
dataPath =: sdsFocus "data_path.json" $ jsonFileStore "TextPicker" False False (?Just "/home/text-fabric-data/github/ETCBC/bhsa/tf/c")

dataSet :: SimpleSDSLens (?DataSet)
dataSet =: sdsFocus "dataSet" memoryShare

loadDataSet :: Task DataSet
loadDataSet =
	get dataSet >>- \data ->
	case data of
		?Just data ->
			return data
		?None ->
			get dataPath >>- \tf_path ->
			accWorld (import_tf (\fn w -> trace_n ("Importing "+++fn+++"...") w) FEATURES tf_path) >>- \data ->
			case data of
				Error e ->
					throw $ concat5 "Failed to load Text-Fabric data from '" tf_path "': " e "."
				Ok data ->
					set (?Just data) dataSet @
					fromJust

lexemeInformation :: SDSLens String (?LexemeInformation) ()
lexemeInformation =: sdsLens
	"lexemeInformation"
	(const ())
	(SDSRead lexemeInformation`)
	(SDSWriteConst \_ _ -> Ok ?None)
	(SDSNotifyWithoutRead \param _ _ predparam -> param == predparam)
	?None
	dataSet

lexemesInformation :: SDSLens [String] [?LexemeInformation] ()
lexemesInformation =: sdsLens
	"lexemesInformation"
	(const ())
	(SDSRead \param mbData -> 'Control.Monad'.sequence (map (\p -> lexemeInformation` p mbData) param))
	(SDSWriteConst \_ _ -> Ok ?None)
	(SDSNotifyWithoutRead \param _ _ predparam -> param == predparam)
	?None
	dataSet

lexemeInformation` :: !String !(?DataSet) -> MaybeError TaskException (?LexemeInformation)
lexemeInformation` _ ?None = Error $ exception "Text-Fabric data has not been loaded"
lexemeInformation` param (?Just data) =
	case get_node_feature_ids ["gloss","lex","voc_lex_utf8"] data of
		?Just [gloss,lex,voc_lex_utf8:_] ->
			case filter_nodes (\_ n _ -> get_node_feature lex n == param) data of
				[|] ->
					Ok ?None
				[|n:_] ->
					Ok $ ?Just
						{ vocalized = get_node_feature voc_lex_utf8 n
						, gloss     = get_node_feature gloss n
						}
		_ ->
			Error $ exception "Text-Fabric data does not contain the required features"
