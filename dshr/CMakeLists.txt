project(dshr Fortran)

set(SRCFILES  dshr_dfield_mod.F90
              dshr_fldlist_mod.F90
              dshr_mod.F90)

foreach(FILE ${SRCFILES})
  if(EXISTS "${CASEROOT}/SourceMods/src.cdeps/${FILE}")
    list(REMOVE_ITEM SRCFILES ${FILE})
    list(APPEND SRCFILES "${CASEROOT}/SourceMods/src.cdeps/${FILE}")
    message("Using ${FILE} from ${CASEROOT}/SourceMods/src.cdeps")
  endif()
endforeach()
add_library(dshr ${SRCFILES})

if(BLD_STANDALONE)
  add_dependencies(dshr cdeps_share)
  target_include_directories (dshr PRIVATE ${CMAKE_BINARY_DIR}/share)
endif()
add_dependencies(dshr streams)

target_include_directories (dshr PRIVATE ${ESMF_F90COMPILEPATHS})
target_include_directories (dshr PRIVATE "${PIO_Fortran_INCLUDE_DIR}")

install(TARGETS dshr
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib)

if(BLD_STANDALONE)
  # ESMX requires mod files
  foreach (SRC ${SRCFILES})
    string(REGEX REPLACE "[.]F90$" ".mod" MOD ${SRC})
    if (NOT DEFINED CIMEROOT AND MOD STREQUAL atm_comp_nuopc.mod)
      set(MOD cdeps_datm_comp.mod)
    endif()
    install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${MOD}" DESTINATION include)
  endforeach ()
endif()
