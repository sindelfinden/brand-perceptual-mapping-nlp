# ----------- __________________
# 01_python_env.R  (Windows 10/11 edition, GPU / CUDA version)
# ----------- __________________

library(reticulate)

setup_python_sbert <- function(envname = "r-sbert-gpu", python_version = "3.10") {
    
    # Потиска предупреждението на Hugging Face за symlink caching под Windows
    Sys.setenv(HF_HUB_DISABLE_SYMLINKS_WARNING = "1")
    
    # ------------------------------------------------------------------
    # 1. Проверка за conda
    #    За GPU workflow под Windows предпочитаме conda среда.
    # ------------------------------------------------------------------
    use_conda <- tryCatch({
        conda_binary()
        TRUE
    }, error = function(e) FALSE)
    
    if (!use_conda) {
        stop(
            "Conda was not found.\n",
            "For the GPU workflow, please install Miniconda or Anaconda first.\n",
            "Then create the environment '", envname, "' and install CUDA-enabled PyTorch in it."
        )
    }
    
    existing <- tryCatch(conda_list()$name, error = function(e) character(0))
    
    if (!(envname %in% existing)) {
        stop(
            "The conda environment '", envname, "' does not exist.\n",
            "Create it first, then install:\n",
            "1. CUDA-enabled torch\n",
            "2. sentence-transformers==2.7.0\n",
            "Current script will not auto-create a GPU environment to avoid accidental CPU-only installs."
        )
    }
    
    # ------------------------------------------------------------------
    # 2. Заключваме се към правилната conda среда
    # ------------------------------------------------------------------
    use_condaenv(envname, required = TRUE)
    
    # За диагностика
    cfg <- py_config()
    py_exe <- cfg$python
    
    # ------------------------------------------------------------------
    # 3. Проверка за налични модули
    # ------------------------------------------------------------------
    if (!py_module_available("sentence_transformers")) {
        stop(
            "The module 'sentence_transformers' is not installed in environment '", envname, "'.\n",
            "Install it with:\n",
            "reticulate::py_install(\n",
            "  packages = 'sentence-transformers==2.7.0',\n",
            "  envname = '", envname, "',\n",
            "  method = 'auto',\n",
            "  pip = TRUE\n",
            ")"
        )
    }
    
    if (!py_module_available("torch")) {
        stop(
            "The module 'torch' is not installed in environment '", envname, "'.\n",
            "Install a CUDA-enabled build, not a CPU-only build."
        )
    }
    
    # ------------------------------------------------------------------
    # 4. Импортиране на модулите
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
        error = function(e) stop(
            "torch cannot be imported.\n",
            "Try restarting the R session and running the script again.\n",
            "Details: ", conditionMessage(e)
        )
    )
    
    # ------------------------------------------------------------------
    # 5. Проверка за CUDA build и реален GPU достъп
    # ------------------------------------------------------------------
    torch_version <- tryCatch(
        py_eval("__import__('torch').__version__"),
        error = function(e) "version unavailable"
    )
    
    torch_cuda_version <- tryCatch(
        py_eval("__import__('torch').version.cuda"),
        error = function(e) NULL
    )
    
    cuda_available <- tryCatch(
        py_eval("__import__('torch').cuda.is_available()"),
        error = function(e) FALSE
    )
    
    device_count <- tryCatch(
        py_eval("__import__('torch').cuda.device_count()"),
        error = function(e) 0L
    )
    
    gpu_name <- tryCatch(
        if (isTRUE(cuda_available) && device_count >= 1L) {
            py_eval("__import__('torch').cuda.get_device_name(0)")
        } else {
            NA_character_
        },
        error = function(e) NA_character_
    )
    
    if (is.null(torch_cuda_version)) {
        stop(
            "PyTorch is installed, but it is a CPU-only build.\n",
            "Detected torch version: ", torch_version, "\n",
            "You must install a CUDA-enabled build in environment '", envname, "'."
        )
    }
    
    if (!isTRUE(cuda_available)) {
        stop(
            "CUDA-enabled PyTorch is installed, but torch.cuda.is_available() is FALSE.\n",
            "Detected torch version: ", torch_version, "\n",
            "Detected torch CUDA version: ", torch_cuda_version, "\n",
            "This usually indicates a compatibility or environment issue."
        )
    }
    
    # ------------------------------------------------------------------
    # 6. Версии за приложението
    # ------------------------------------------------------------------
    get_version <- function(module_name) {
        tryCatch(
            py_eval(paste0("__import__('", module_name, "').__version__")),
            error = function(e) "version unavailable"
        )
    }
    
    py_versions <- list(
        python_executable      = py_exe,
        sentence_transformers  = get_version("sentence_transformers"),
        torch                  = torch_version,
        torch_cuda             = torch_cuda_version,
        transformers           = get_version("transformers"),
        cuda_available         = cuda_available,
        device_count           = device_count,
        gpu_name               = gpu_name
    )
    
    if (!dir.exists("outputs/tables")) {
        dir.create("outputs/tables", recursive = TRUE)
    }
    
    saveRDS(py_versions, "outputs/tables/python_versions.rds")
    
    message("Python environment ready: ", envname)
    message("Python executable: ", py_exe)
    message("sentence-transformers: ", py_versions$sentence_transformers)
    message("torch: ", py_versions$torch)
    message("torch CUDA version: ", py_versions$torch_cuda)
    message("CUDA available: ", py_versions$cuda_available)
    message("Device count: ", py_versions$device_count)
    if (!is.na(py_versions$gpu_name)) {
        message("GPU name: ", py_versions$gpu_name)
    }
    
    list(
        st = st,
        torch = torch,
        python_executable = py_exe,
        cuda_available = cuda_available,
        gpu_name = gpu_name
    )
}

py_mods <- setup_python_sbert()
py_config()