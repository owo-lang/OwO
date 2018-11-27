#ifndef __IMPOSSIBLE_H__
#define __IMPOSSIBLE_H__

import OwO.Util.Impossible
#define __IMPOSSIBLE__ (throwImpossible $ Impossible __FILE__ __LINE__)
#define __UNREACHABLE__ (throwImpossible $ Unreachable __FILE__ __LINE__)
#define __TODO__ (throwImpossible $ Unimplemented __FILE__ __LINE__)

#endif /* !__IMPOSSIBLE_H__ */
