!general_specmod.f90:
subroutine spwget
#ifdef USE_SP_MOD
   use sp_mod
#endif
end subroutine spwget
subroutine spffte
#ifdef USE_SP_MOD
   use sp_mod
#endif
end subroutine spffte
subroutine splat
#ifdef USE_SP_MOD
   use sp_mod
#endif
end subroutine splat
subroutine splegend
#ifdef USE_SP_MOD
   use sp_mod
#endif
end subroutine splegend
!general_transform.f90: general_sptranf_s_:
subroutine spsynth
#ifdef USE_SP_MOD
   use sp_mod
#endif
end subroutine spsynth
subroutine spanaly
#ifdef USE_SP_MOD
   use sp_mod
#endif
end subroutine spanaly
!general_transform.f90: general_sptranf_v_:
subroutine spdz2uv
#ifdef USE_SP_MOD
   use sp_mod
#endif
end subroutine spdz2uv
subroutine spuv2dz
#ifdef USE_SP_MOD
   use sp_mod
#endif
end subroutine spuv2dz
