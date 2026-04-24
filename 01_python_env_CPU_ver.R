# ----------- __________________
# 01_python_env.R  (Windows 10/11 edition)
# ----------- __________________
library(reticulate)

setup_python_sbert <- function(envname = "r-sbert", python_version = "3.10") {
    
    # ------------------------------------------------------------------
    # 1. На Windows virtualenv понякога не се открива коректно по име.
    #    Използваме condaenv ако conda е налична, иначе virtualenv.
    # ------------------------------------------------------------------
    use_conda <- tryCatch({
        conda_binary()   # проверява дали conda е инсталирана
        TRUE
    }, error = function(e) FALSE)
    
    if (use_conda) {
        # --- CONDA path (Anaconda / Miniconda) ---
        existing <- tryCatch(conda_list()$name, error = function(e) character(0))
        
        if (!(envname %in% existing)) {
            conda_create(
                envname = envname,
                python_version = python_version
            )
        }
        use_condaenv(envname, required = TRUE)
        
        if (!py_module_available("sentence_transformers")) {
            conda_install(
                envname  = envname,
                packages = "pip",          # уверяваме се, че pip е наличен
                channel  = "defaults"
            )
            py_install(
                packages = "sentence-transformers==2.7.0",
                envname  = envname,
                method   = "conda",
                pip      = TRUE
            )
        }
        
    } else {
        # --- VIRTUALENV път (без conda) ---
        # Windows изисква python.exe да е в PATH.
        # reticulate търси python автоматично; задаваме го явно за сигурност.
        python_path <- Sys.which("python")
        if (nchar(python_path) == 0) {
            stop(
                "Python was not found in PATH.\n",
                "Install Python 3.10 от https://www.python.org/downloads/\n",
                "and check the box next to 'Add Python to PATH' during installation."
            )
        }
        
        existing_envs <- tryCatch(virtualenv_list(), error = function(e) character(0))
        
        if (!(envname %in% existing_envs)) {
            virtualenv_create(
                envname = envname,
                python  = python_path      # явен път — задължително на Windows
            )
        }
        use_virtualenv(envname, required = TRUE)
        
        if (!py_module_available("sentence_transformers")) {
            py_install(
                packages = "sentence-transformers==2.7.0",
                envname  = envname,
                method   = "virtualenv",
                pip      = TRUE
            )
        }
    }
    
    # ------------------------------------------------------------------
    # 2. Импортиране на модули
    #    На Windows torch невинаги се импортира коректно при първи опит
    #    след инсталация — добавяме try() за по-четим error message.
    # ------------------------------------------------------------------
    st <- tryCatch(
        import("sentence_transformers"),
        error = function(e) stop(
            "sentence_transformers cannot be imported.\n",
            "Try restarting the R session and running the script again.\n",
            "Details: ", conditionMessage(e)
        )
    )
    
    torch <- tryCatch(
        import("torch"),
        error = function(e) {
            message("torch not found — the CPU version will be installed.")
            py_install(
                packages = "torch==2.3.0",   # CPU-only wheel за Windows
                envname  = envname,
                method   = "auto",
                pip      = TRUE
            )
            import("torch")
        }
    )
    
    # -------------------------------------------------------------------------
    # 3. Версии за приложението
    #    py_eval() Windows sometimes requires an explicit ; between statements.
    # -------------------------------------------------------------------------
    get_version <- function(module_name) {
        tryCatch(
            py_eval(paste0("__import__('", module_name, "').__version__")),
            error = function(e) "version unavailable"
        )
    }
    
    py_versions <- list(
        sentence_transformers = get_version("sentence_transformers"),
        torch                 = get_version("torch"),
        transformers          = get_version("transformers")
    )
    
    # Създаваме директорията ако не съществува (Windows не я създава автоматично)
    if (!dir.exists("outputs/tables")) {
        dir.create("outputs/tables", recursive = TRUE)
    }
    saveRDS(py_versions, "outputs/tables/python_versions.rds")
    
    message("Python environment ready: ", envname)
    message("sentence-transformers: ", py_versions$sentence_transformers)
    message("torch: ", py_versions$torch)
    
    list(st = st, torch = torch)
}

py_mods <- setup_python_sbert()
py_config()




# library(reticulate)
# 
# # 1. Заключваме се към правилната среда
# use_condaenv("r-sbert", required = TRUE)
# 
# # 2. Инсталираме numpy първо (базова зависимост)
# py_install(
#     packages = "numpy",
#     envname  = "r-sbert",
#     method   = "conda",
#     pip      = FALSE        # conda wheel за numpy е по-стабилен на Windows
# )
# 
# # 3. Инсталираме torch (CPU версия — по-лека, достатъчна за embeddings)
# py_install(
#     packages = "torch==2.3.0",
#     envname  = "r-sbert",
#     method   = "conda",
#     pip      = TRUE
# )
# 
# # 4. Инсталираме sentence-transformers
# py_install(
#     packages = "sentence-transformers==2.7.0",
#     envname  = "r-sbert",
#     method   = "conda",
#     pip      = TRUE
# )
# 
# # 5. Проверка
# py_config()
# py_module_available("numpy")
# py_module_available("sentence_transformers")
# py_module_available("torch")




