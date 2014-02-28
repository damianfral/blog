-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid
import Control.Monad
import Hakyll
import Text.Pandoc
import System.FilePath.Posix

-------------------------------------------------------------------------------

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
	{ feedTitle       = "damianfral's blog"
	, feedDescription = "Damián Franco Álvarez's blog"
	, feedAuthorName  = "Damián Franco Álvarez"
	, feedAuthorEmail = "huevofritopamojarpan@gmail.com"
	, feedRoot        = "http://damianfral.github.io/blog"
	}

-------------------------------------------------------------------------------

pandocOptions :: WriterOptions
pandocOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = MathML Nothing }


-------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
	match "images/**" $ do
		route   idRoute
		compile copyFileCompiler

	match "js/*" $ do
		route   idRoute
		compile copyFileCompiler

	match "css/*.css" $ do
		route   idRoute
		compile compressCssCompiler

	--match "css/*.styl" $ do
	--	route   $ setExtension "css"
	--	compile stylusCompiler

	match "fonts/*" $ do
		route   idRoute
		compile copyFileCompiler

	--match (fromList ["about.rst", "contact.markdown"]) $ do
	--	route   $ setExtension "html"
	--	compile $ pandocCompiler
	--		>>= loadAndApplyTemplate "templates/default.html" defaultContext
	--		>>= relativizeUrls


	match "posts/*" $ do
		route $ setExtension "html"
		compile $ pandocCompilerWith defaultHakyllReaderOptions pandocOptions
			>>= loadAndApplyTemplate "templates/post.html"    postCtx
			-- save a snapshot to be used later in rss generation
			>>= saveSnapshot "content"
			>>= loadAndApplyTemplate "templates/default.html" postCtx
			>>= relativizeUrls

	match "index.html" $ do
		route idRoute
		compile $ do
			let indexCtx = field "posts" $ const  $ postList recentFirst
			getResourceBody
				>>= applyAsTemplate indexCtx
				>>= loadAndApplyTemplate "templates/default.html" postCtx
				>>= relativizeUrls

	match "templates/*" $ compile templateCompiler

	--create ["archive.html"] $ do
	--    route idRoute
	--    compile $ do
	--        let archiveCtx =
	--                field "posts" (\_ -> postList recentFirst) `mappend`
	--                constField "title" "Archives"              `mappend`
	--                defaultContext

	--        makeItem ""
	--            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
	--            >>= loadAndApplyTemplate "templates/default.html" archiveCtx
	--            >>= relativizeUrls

	create ["atom.xml"] $ do
		route idRoute
		compile $ do
			let feedCtx = postCtx <> bodyField "description"
			posts <- fmap (take 10) . recentFirst =<<
				loadAllSnapshots "posts/*" "content"
			renderAtom myFeedConfiguration feedCtx posts


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
	dateField "date" "%d/%m/%Y" <> defaultContext <> metaKeywordContext


--------------------------------------------------------------------------------
postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = do
	posts   <- sortFilter =<< loadAll "posts/*"
	itemTpl <- loadBody "templates/post-item.html"
	applyTemplateList itemTpl postCtx posts

niceRoute :: Routes
niceRoute = customRoute createIndexRoute
	where
		createIndexRoute ident = takeBaseName (toFilePath ident) </> "index.html"


stylusCompiler :: Compiler (Item String)
stylusCompiler = liftM (fmap compressCss) $ getResourceString >>= withItemBody  (unixFilter "stylus" ["-p", "-u nib"])

-- metaKeywordContext will return a Context containing a String
metaKeywordContext :: Context String
-- can be reached using $metaKeywords$ in the templates
-- Use the current item (markdown file)
metaKeywordContext = field "metaKeywords" $ \item -> do
	-- tags contains the content of the "tags" metadata
	-- inside the item (understand the source)
	tags <- getMetadataField (itemIdentifier item) "tags"
	-- if tags is empty return an empty string
	-- in the other case return
	--   <meta name="keywords" content="$tags$">
	return $ maybe "" showMetaTags tags
		where
			showMetaTags t = "<meta name=\"keywords\" content=\"" ++ t ++ "\">\n"