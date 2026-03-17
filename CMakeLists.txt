cmake_minimum_required(VERSION 3.16)
project(zeus-parent)
add_subdirectory(parser)
add_subdirectory(compiler)

add_subdirectory(zeus-lsp)


include(InstallRequiredSystemLibraries)
set(CPACK_PACKAGE_NAME "zeus")
set(CPACK_PACKAGE_VENDOR "hisoka999")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "compiler and language server for the zeus programming language")
set(CPACK_PACKAGE_VERSION_MAJOR ${WIRTHX_VERSION_MAJOR})
set(CPACK_PACKAGE_VERSION_MINOR ${WIRTHX_VERSION_MINOR})
set(CPACK_PACKAGE_VERSION_PATCH ${WIRTHX_VERSION_PATCH})
set(CPACK_PACKAGE_CONTACT "hisoka999@gmail.com")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")


# Windows ZIP-Paket
set(CPACK_GENERATOR "ZIP")
if (WIN32)
    set(CPACK_GENERATOR "ZIP")
endif ()

# Linux DEB-Paket
if (UNIX AND NOT APPLE)
    SET(CPACK_GENERATOR "DEB;TGZ")
    set(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)
    set(CPACK_DEBIAN_PACKAGE_MAINTAINER "Stefan Lüdtke <hisoka999@gmail.com>")
    set(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6 (>= 2.27) , binutils, gcc")
    set(CPACK_DEBIAN_PACKAGE_SECTION "devel")
    set(CPACK_DEBIAN_PACKAGE_HOMEPAGE "https://zeus-language.github.io/")

endif ()

install(TARGETS zeus DESTINATION bin)
install(TARGETS zeusls DESTINATION bin)
install(DIRECTORY stdlib/ DESTINATION include/zeus/stdlib/)

include(CPack)