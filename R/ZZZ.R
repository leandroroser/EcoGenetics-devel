#' @keywords internal


.onAttach <- function(...) {
	
	vers <- utils::packageDescription("EcoGenetics", fields = "Version")
  
	textstart<- paste("\n\n",
										"\n", "                ---------------------", "\n",
										"\r                  ","||","EcoGenetics","||",
										"\n", "                ---------------------", "\n",
										"\n", "  Version",  vers, "\n",
										"  GitHub: ecogenetics_devel()", "\n",
										"  Online tutorial: ecogenetics_tutorial()", "\n",
										"  Comments / suggestions / bug reports: learoser@gmail.com \n",
										"  Overview: ?EcoGenetics\n")
	
	packageStartupMessage(textstart)
}
