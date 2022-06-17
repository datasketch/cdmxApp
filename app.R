# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
cdmxApp::run_app() # add parameters here (if any)

#ckanConf=140e35f9-9244-4b45-b638-816c2ab7651a