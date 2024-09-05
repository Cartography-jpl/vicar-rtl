Summary: This is the VICAR RTL library
Name: vicar-rtl
Version: 1.10
Release: 1.el%{rhel}
License: Copyright 2021 California Institute of Technology ALL RIGHTS RESERVED
Group: Applications/Engineering
Vendor: California Institute of Technology
URL: http://www-mipl.jpl.nasa.gov/external/vicar.html
Source0: %{name}-%{version}.tar.gz
BuildRoot: %{_tmppath}/%{name}-%{version}-%{release}-root
BuildRequires: ncurses-devel
Prefix: /opt/afids_support

%description

In the original MIPL code, the libraries libp2, libp1, librtl, libshvic, and 
libtae are separate libraries. However, they are never used except together. 
As a convenience, we put them all into one library. We also call this library
"libvicar_rtl" rather than "librtl" because there are other libraries 
that use the same "librtl" library name. We also stuff libmath77 into this
library, since again we don't need to use this library independently.

%prep
%setup -q

%build
./configure --prefix=/opt/afids_support 
make %_smp_mflags 

%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install
rm -f $RPM_BUILD_ROOT/opt/afids_support/lib/libvicar_rtl.la

%clean
rm -rf $RPM_BUILD_ROOT


%files
%defattr(-,root,root,-)
%doc
/opt/afids_support/bin/v2param
/opt/afids_support/lib/libvicar_rtl.a
/opt/afids_support/lib/libvicar_rtl.so
/opt/afids_support/lib/libvicar_rtl.so.1
/opt/afids_support/lib/libvicar_rtl.so.1.0.10
/opt/afids_support/include/vicar_rtl/*
/opt/afids_support/lib/pkgconfig/vicar-rtl.pc

%changelog
* Thu Apr  8 2021 Smyth <smyth@macsmyth> - 1.10-1.el%{rhel}
- Collect various bug fixes into a new version.

* Wed Sep 25 2019 Smyth <smyth@macsmyth> - 1.09-1.el%{rhel}
- Various fixes to build on conda on the mac. No user visible changes.

* Thu Jul 19 2018 Smyth <smyth@macsmyth> - 1.08-1.el%{rhel}
- Add zifmessage.h header file

* Fri Oct 7 2016 Mike M Smyth <smyth@pistol> - 1.07-1
- Update stacka and add stacka_big

* Thu Dec 17 2015 Mike M Smyth <smyth@pistol> - 1.06-2
- Rebuild

* Mon Sep 29 2014 Mike M Smyth <smyth@pistol> - 1.06-1
- Just rearranged some files

* Thu Oct 10 2013 Mike M Smyth <smyth@pistol> - 1.05-1
- Have output label initialized to all nulls.

* Wed Sep  4 2013 Mike M Smyth <smyth@pistol> - 1.04-1
- Add build of v2param, which is needed by AFIDS xvd to print out an
  image. Also add a special backdoor in history information for
  testing.  We have the USER and DAT_TIM fields of the vicar HISTORY
  label set to the value of the environment variables
  AFIDS_FAKE_TEST_USER and AFIDS_FAKE_TEST_DAT_TIM so these can be
  fixed values regardless of who runs the tests (without these normal 
  user name and date/time are used).

* Fri Aug 16 2013 Mike M Smyth <smyth@pistol> - 1.03-1
- Add new p2 functions get_seconds_big and rangen_big. This is needed
  by the updated version of gausnoise that Ray will deliver shortly.

* Thu Jun 20 2013 Mike M Smyth <smyth@pistol> - 1.02-1
- Minor fix for building on redhat 5.5

* Thu Jun  6 2013 Mike M Smyth <smyth@pistol> - 1.01-1
- Add new function stacka_big

* Fri Nov 16 2012 Mike M Smyth <smyth@pistol> - 
- Initial build.

