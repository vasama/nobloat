#pragma once

#include <algorithm>
#include <iterator>
#include <memory>
#include <new>
#include <utility>
#include <vector>

#include <cassert>
#include <cstddef>
#include <cstring>

#ifndef nobloat_CONFIG_DISABLE_INLINE_HINTS
#if defined(__clang__) || defined(__GNUC__)
#define nobloat_INLINE __attribute__((always_inline))
#define nobloat_NOINLINE __attribute__((noinline))
#elif defined(_MSC_VER)
#define nobloat_INLINE __forceinline
#define nobloat_NOINLINE __declspec(noinline)
#endif
#endif

#ifndef nobloat_INLINE
#define nobloat_INLINE
#define nobloat_NOINLINE
#endif

#if defined(__clang__) || defined(__GNUC__)
#define nobloat_LIKELY(...) __builtin_expect((__VA_ARGS__), 1)
#define nobloat_UNLIKELY(...) __builtin_expect((__VA_ARGS__), 0)
#elif defined(_MSC_VER)
#define nobloat_LIKELY(...) (__VA_ARGS__)
#define nobloat_UNLIKELY(...) (__VA_ARGS__)
#endif

namespace nobloat {
namespace detail {

using std::byte;

template<typename T>
using const_param = std::conditional<
	std::is_trivially_copyable_v<T> &&
	sizeof(T) <= sizeof(void*), const T, const T&>;

template<typename T>
using const_param_t = typename const_param<T>::type;

template<typename T>
using is_trivially_relocatable = std::is_trivially_copyable<T>;

template<typename T>
using allocator_param = std::conditional_t<std::is_empty_v<T>, T, T&>;

template<typename T>
using allocator_param_t = typename allocator_param<T>::type;

template<typename T>
constexpr bool is_trivially_relocatable_v = is_trivially_relocatable<T>::value;

template<typename T>
auto is_iterator_2(T it)
	-> decltype(it != it, *it, ++it, std::true_type{});

template<typename T>
auto is_iterator_1(int) -> decltype(is_iterator_2(std::declval<T>()));

template<typename>
std::false_type is_iterator_1(...);

template<typename T>
using is_iterator = decltype(is_iterator_1<T>(0));

template<typename T>
constexpr bool is_iterator_v = is_iterator<T>::value;

template<typename T>
void relocate(T* dst, T* src_beg, T* src_end)
{
	if constexpr (is_trivially_relocatable_v<T>)
	{
		std::memcpy(
			reinterpret_cast<byte*>(dst),
			reinterpret_cast<byte*>(src_beg),
			reinterpret_cast<byte*>(src_end) -
				reinterpret_cast<byte*>(src_beg));
	}
	else
	{
		for (; src_beg != src_end; ++dst, ++src_beg)
		{
			new(dst) T(std::move(*src_beg));
			src_beg->~T();
		}
	}
}

template<typename T>
void relocate_left(T* dst, T* src_beg, T* src_end)
{
	if constexpr (is_trivially_relocatable_v<T>)
	{
		std::memmove(
			reinterpret_cast<byte*>(dst),
			reinterpret_cast<byte*>(src_beg),
			reinterpret_cast<byte*>(src_end) -
				reinterpret_cast<byte*>(src_beg));
	}
	else
	{
		for (; src_beg != src_end; ++dst, ++src_beg)
		{
			new(dst) T(std::move(*src_beg));
			src_beg->~T();
		}
	}
}

template<typename T>
void relocate_right(T* dst, T* src_beg, T* src_end)
{
	if constexpr (is_trivially_relocatable_v<T>)
	{
		std::memmove(
			reinterpret_cast<byte*>(dst),
			reinterpret_cast<byte*>(src_beg),
			reinterpret_cast<byte*>(src_end) -
				reinterpret_cast<byte*>(src_beg));
	}
	else
	{
		for (dst += src_end - src_beg; src_end-- != src_beg;)
		{
			new(--dst) T(std::move(*src_end));
			src_end->~T();
		}
	}
}

struct core
{
	byte* beg;
	byte* mid;
	byte* end;
};

template<bool>
struct capacity_wrapper
{
	constexpr capacity_wrapper(size_t /*capacity*/)
	{
	}
};

template<>
struct capacity_wrapper<true>
{
	size_t capacity;

	constexpr capacity_wrapper(size_t capacity)
		: capacity(capacity)
	{
	}
};

template<bool HasLocalStorage>
bool is_dynamic(core* array, byte* beg)
{
	if constexpr (HasLocalStorage)
	{
		return beg != reinterpret_cast<byte*>(array + 1);
	}
	else
	{
		return beg != nullptr;
	}
}

template<bool HasLocalStorage>
void construct(core* array, capacity_wrapper<HasLocalStorage> local_capacity)
{
	if constexpr (HasLocalStorage)
	{
		byte* storage = reinterpret_cast<byte*>(array + 1);

		array->beg = storage;
		array->mid = storage;
		array->end = storage + local_capacity.capacity;
	}
	else
	{
		array->beg = nullptr;
		array->mid = nullptr;
		array->end = nullptr;
	}
}

template<typename DestroyT, typename Allocator, bool HasLocalStorage>
void destroy(core* array, allocator_param<Allocator> allocator)
{
	byte* beg = array->beg;
	byte* mid = array->mid;

	std::destroy(
		reinterpret_cast<DestroyT*>(beg),
		reinterpret_cast<DestroyT*>(mid));

	if (is_dynamic<HasLocalStorage>(array, beg))
	{
		allocator.deallocate(beg, array->end - beg);
	}
}

template<typename RelocateT, typename Allocator, bool HasLocalStorage>
struct operations
{
	typedef allocator_param<Allocator> allocator_param_type;

	template<typename DestroyT, typename CopyT>
	static void copy_assign(core* array,
		const core* src, allocator_param_type allocator)
	{
		byte* beg = array->beg;
		byte* mid = array->mid;

		std::destroy(
			reinterpret_cast<DestroyT*>(beg),
			reinterpret_cast<DestroyT*>(mid));

		array->mid = beg;

		push_back_range(array,
			reinterpret_cast<const CopyT*>(src->beg),
			reinterpret_cast<const CopyT*>(src->mid), allocator);
	}

	template<typename DestroyT, typename CopyT>
	static void copy_assign_allocator(core* array, core* src,
		Allocator& allocator, const_param_t<Allocator> src_allocator,
		capacity_wrapper<HasLocalStorage> local_capacity)
	{
		byte* beg = array->beg;
		byte* mid = array->mid;

		std::destroy(
			reinterpret_cast<DestroyT*>(beg),
			reinterpret_cast<DestroyT*>(mid));

		if (is_dynamic<HasLocalStorage>(array, beg))
		{
			allocator.deallocate(beg, array->end - beg);
		}

		allocator = src_allocator;
		construct<HasLocalStorage>(array, local_capacity);

		push_back_range(array,
			reinterpret_cast<const CopyT*>(src->beg),
			reinterpret_cast<const CopyT*>(src->mid), allocator);
	}

	template<bool SrcHasLocalStorage>
	static void move_construct(core* array,
		core* src, allocator_param_type allocator,
		capacity_wrapper<HasLocalStorage> local_capacity,
		capacity_wrapper<SrcHasLocalStorage> src_local_capacity)
	{
		byte* srcbeg = src->beg;
		byte* srcmid = src->mid;

		if (srcbeg == srcmid)
		{
			construct<HasLocalStorage>(array, local_capacity);
			return;
		}

		size_t size = srcmid - srcbeg;

		if (is_dynamic<SrcHasLocalStorage>(src, srcbeg))
		{
			byte* srcend = src->end;

			bool adopt = true;
			if constexpr (HasLocalStorage)
				adopt = srcend - srcbeg < local_capacity.capacity;

			if (adopt)
			{
				array->beg = srcbeg;
				array->mid = srcmid;
				array->end = srcend;

				construct<SrcHasLocalStorage>(src, src_local_capacity);

				return;
			}
		}

		bool allocate = true;
		if constexpr (HasLocalStorage)
			allocate = size > local_capacity.capacity;

		if (allocate)
		{
			byte* beg = allocator.allocate(size);

			array->beg = beg;
			array->end = beg + size;
		}

		byte* beg = array->beg;

		relocate(
			reinterpret_cast<RelocateT*>(beg),
			reinterpret_cast<RelocateT*>(srcbeg),
			reinterpret_cast<RelocateT*>(srcmid));

		array->mid = beg + size;
		src->mid = srcbeg;
	}

	template<typename DestroyT, bool SrcHasLocalStorage>
	static void move_assign(core* array,
		core* src, allocator_param_type allocator,
		capacity_wrapper<HasLocalStorage> local_capacity,
		capacity_wrapper<SrcHasLocalStorage> src_local_capacity)
	{
		byte* beg = array->beg;
		byte* mid = array->mid;

		std::destroy(
			reinterpret_cast<DestroyT*>(beg),
			reinterpret_cast<DestroyT*>(mid));

		byte* srcbeg = src->beg;
		byte* srcmid = src->mid;

		if (srcbeg == srcmid)
		{
			array->mid = beg;
			return;
		}

		size_t size = srcmid - srcbeg;

		byte* end = array->end;
		if (is_dynamic<SrcHasLocalStorage>(src, srcbeg))
		{
			byte* srcend = src->end;

			bool adopt = true;
			if constexpr (HasLocalStorage)
				adopt = srcend - srcbeg < local_capacity.capacity;

			if (adopt)
			{
				if (is_dynamic<HasLocalStorage>(array, beg))
				{
					allocator.deallocate(beg, end - beg);
				}

				array->beg = srcbeg;
				array->mid = srcmid;
				array->end = srcend;

				construct<SrcHasLocalStorage>(src, src_local_capacity);

				return;
			}
		}

		size_t capacity = end - beg;
		if (size > capacity)
		{
			allocator.deallocate(beg, capacity);
			beg = allocator.allocate(size);

			array->beg = beg;
			array->end = beg + size;
		}

		relocate(
			reinterpret_cast<RelocateT*>(beg),
			reinterpret_cast<RelocateT*>(srcbeg),
			reinterpret_cast<RelocateT*>(srcmid));

		array->mid = beg + size;
		src->mid = srcbeg;
	}

	template<typename DestroyT>
	static void move_assign_allocator(core* array, core* src,
		Allocator& allocator, Allocator& src_allocator,
		capacity_wrapper<HasLocalStorage> local_capacity)
	{
		byte* beg = array->beg;
		byte* mid = array->mid;

		std::destroy(
			reinterpret_cast<DestroyT*>(beg),
			reinterpret_cast<DestroyT*>(mid));

		if (is_dynamic<HasLocalStorage>(array, beg))
		{
			allocator.deallocate(beg, array->end - beg);
		}

		allocator = std::move(src_allocator);
		construct<HasLocalStorage>(array, local_capacity);

		move_elements(array, src, allocator);
	}

	template<typename DestroyT>
	static void move_assign_elements(core* array,
		core* src, allocator_param_type allocator)
	{
		byte* beg = array->beg;
		byte* mid = array->mid;

		std::destroy(
			reinterpret_cast<DestroyT*>(beg),
			reinterpret_cast<DestroyT*>(mid));

		array->mid = array->beg;

		move_elements(array, src, allocator);
	}

	static void move_elements(core* array,
		core* src, allocator_param_type allocator)
	{
		byte* src_beg = src->beg;
		byte* src_mid = src->mid;

		byte* slots = push_back(array, src_mid - src_beg, allocator);

		relocate(
			reinterpret_cast<RelocateT*>(slots),
			reinterpret_cast<RelocateT*>(src_beg),
			reinterpret_cast<RelocateT*>(src_mid));

		src->mid = src_beg;
	}

	static byte* expand(core* array,
		size_t min_capacity, allocator_param_type allocator)
	{
		byte* beg = array->beg;
		byte* mid = array->mid;
		byte* end = array->end;

		size_t capacity = end - beg;
		size_t new_capacity = std::max(min_capacity, capacity * 2);

		byte* new_beg = reinterpret_cast<byte*>(
			allocator.allocate(new_capacity));
		byte* new_end = new_beg + new_capacity;

		if (beg != mid)
		{
			relocate(
				reinterpret_cast<RelocateT*>(new_beg),
				reinterpret_cast<RelocateT*>(beg),
				reinterpret_cast<RelocateT*>(mid));
		}

		byte* empty = nullptr;
		if constexpr (HasLocalStorage)
			empty = reinterpret_cast<byte*>(array + 1);

		if (beg != empty)
		{
			allocator.deallocate(beg, capacity);
		}

		array->beg = new_beg;
		array->mid = new_beg + (mid - beg);
		array->end = new_end;

		return new_beg;
	}

	static nobloat_NOINLINE byte* push_back_slow_path(
		core* array, size_t size, allocator_param_type allocator)
	{
		size_t cur_size = array->mid - array->beg;
		size_t new_size = cur_size + size;

		byte* new_beg = expand(array, new_size, allocator);

		array->mid = new_beg + new_size;
		return new_beg + cur_size;
	}

	static byte* push_back(core* array,
		size_t size, allocator_param_type allocator)
	{
		byte* mid = array->mid;
		byte* new_mid = mid + size;

		if (nobloat_UNLIKELY(new_mid > array->end))
		{
			return push_back_slow_path(array, size, allocator);
		}

		array->mid = new_mid;
		return mid;
	}

	template<typename CopyT>
	static void push_back_range(core* array,
		const CopyT* first, const CopyT* last, allocator_param_type allocator)
	{
		byte* slots = push_back(array,
			reinterpret_cast<const byte*>(last) -
				reinterpret_cast<const byte*>(first), allocator);

		std::uninitialized_copy(first, last, reinterpret_cast<CopyT*>(slots));
	}

	template<typename T, typename InputIt>
	static void push_back_range(core* array,
		InputIt first, InputIt last, allocator_param_type allocator)
	{
		byte* slots = push_back(array,
			std::distance(first, last) * sizeof(T), allocator);

		std::uninitialized_copy(first, last, reinterpret_cast<T*>(slots));
	}
	
	static nobloat_NOINLINE byte* insert_slow_path(
		core* array, byte* pos, size_t size, allocator_param_type allocator)
	{
		byte* beg = array->beg;

		size_t offset = pos - beg;
		size_t cur_size = array->mid - beg;
		size_t new_size = cur_size + size;

		byte* new_beg = expand(array, new_size, allocator);

		byte* new_pos = new_beg + offset;

		relocate_right(
			reinterpret_cast<RelocateT*>(new_pos + size),
			reinterpret_cast<RelocateT*>(new_pos),
			reinterpret_cast<RelocateT*>(new_beg + cur_size));

		array->mid = new_beg + new_size;
		return new_pos;
	}

	static byte* insert(core* array,
		byte* pos, size_t size, allocator_param_type allocator)
	{
		byte* mid = array->mid;
		byte* new_mid = mid + size;

		if (nobloat_UNLIKELY(new_mid > array->end))
		{
			return insert_slow_path(array, pos, size, allocator);
		}

		if (pos != mid)
		{
			relocate_right(
				reinterpret_cast<RelocateT*>(pos + size),
				reinterpret_cast<RelocateT*>(pos),
				reinterpret_cast<RelocateT*>(mid));
		}

		array->mid = new_mid;
		return pos;
	}

	template<typename CopyT>
	static byte* insert_range(core* array, byte* pos,
		const CopyT* first, const CopyT* last, allocator_param_type allocator)
	{
		byte* slots = insert(array, pos,
			reinterpret_cast<const byte*>(last) -
				reinterpret_cast<const byte*>(first), allocator);

		std::uninitialized_copy(first, last, reinterpret_cast<CopyT*>(slots));

		return slots;
	}

	template<typename T, typename InputIt>
	static byte* insert_range(core* array, byte* pos,
		InputIt first, InputIt last, allocator_param_type allocator)
	{
		byte* slots = insert(array, pos,
			std::distance(first, last) * sizeof(T), allocator);

		std::uninitialized_copy(first, last, reinterpret_cast<T*>(slots));

		return slots;
	}

	template<bool Construct, typename ConstructT, typename DestroyT>
	static byte* resize(core* array,
		size_t new_size, allocator_param_type allocator)
	{
		byte* beg = array->beg;
		byte* mid = array->mid;
		byte* new_mid = beg + new_size;

		if (nobloat_LIKELY(new_mid > mid))
		{
			if (new_mid > array->end)
			{
				byte* new_beg = expand(array, new_size, allocator);

				mid = new_beg + (mid - beg);
				new_mid = new_beg + new_size;
			}

			array->mid = new_mid;

			if constexpr (Construct)
			{
				std::uninitialized_value_construct(
					reinterpret_cast<ConstructT*>(mid),
					reinterpret_cast<ConstructT*>(new_mid));
			}

			return mid;
		}
		else if (nobloat_LIKELY(new_mid < mid))
		{
			array->mid = new_mid;

			std::destroy(
				reinterpret_cast<DestroyT*>(new_mid),
				reinterpret_cast<DestroyT*>(mid));

			return new_mid;
		}

		return mid;
	}

	static void reserve(core* array,
		size_t min_capacity, allocator_param_type allocator)
	{
		if ((size_t)(array->end - array->beg) < min_capacity)
		{
			expand(array, min_capacity, allocator);
		}
	}

	static void shrink_to_fit(core* array, allocator_param_type allocator,
		capacity_wrapper<HasLocalStorage> local_capacity)
	{
		byte* mid = array->mid;
		byte* end = array->end;

		if (mid == end) return;

		byte* beg = array->beg;
		size_t capacity = end - beg;
		size_t new_capacity = mid - beg;

		byte* new_beg;
		if constexpr (HasLocalStorage)
		{
			if (capacity <= local_capacity.capacity)
				return;

			if (new_capacity <= local_capacity.capacity)
			{
				new_beg = reinterpret_cast<byte*>(array + 1);
			}
			else
			{
				new_beg = allocator.allocate(new_capacity);
			}
		}
		else
		{
			if (new_capacity > 0)
			{
				new_beg = allocator.allocate(new_capacity);
			}
			else
			{
				allocator.deallocate(beg, capacity);

				array->beg = nullptr;
				array->mid = nullptr;
				array->end = nullptr;

				return;
			}
		}

		relocate(
			reinterpret_cast<RelocateT*>(new_beg),
			reinterpret_cast<RelocateT*>(beg),
			reinterpret_cast<RelocateT*>(mid));

		allocator.deallocate(beg, capacity);

		byte* new_end = new_beg + new_capacity;

		array->beg = new_beg;
		array->mid = new_end;
		array->end = new_end;
	}
};

template<typename DestroyT>
void clear(core* array)
{
	byte* beg = array->beg;
	byte* mid = array->mid;
	array->mid = beg;

	std::destroy(
		reinterpret_cast<DestroyT*>(beg),
		reinterpret_cast<DestroyT*>(mid));
}

template<typename RelocateT, typename DestroyT>
byte* erase(core* array, byte* first, byte* last)
{
	byte* mid = array->mid;
	byte* new_mid = mid - (last - first);

	std::destroy(
		reinterpret_cast<DestroyT*>(first),
		reinterpret_cast<DestroyT*>(last));

	if (last != mid)
	{
		relocate_left(
			reinterpret_cast<RelocateT*>(first),
			reinterpret_cast<RelocateT*>(last),
			reinterpret_cast<RelocateT*>(mid));
	}

	array->mid = new_mid;

	return first;
}

template<typename DestroyT>
void pop_back(core* array, size_t size)
{
	byte* mid = array->mid;
	byte* new_mid = mid - size;
	array->mid = new_mid;

	reinterpret_cast<DestroyT*>(new_mid)->~DestroyT();
}

template<typename T>
bool equal(const core* lhs, const core* rhs)
{
	byte* lhs_beg = lhs->beg;
	byte* lhs_mid = lhs->mid;
	byte* rhs_beg = rhs->beg;
	byte* rhs_mid = rhs->mid;

	if (lhs_mid - lhs_beg != rhs_mid - rhs_beg)
		return false;
	
	return std::equal(
		reinterpret_cast<const T*>(lhs_beg),
		reinterpret_cast<const T*>(lhs_mid),
		reinterpret_cast<const T*>(rhs_beg),
		reinterpret_cast<const T*>(rhs_mid));
}

template<typename T>
bool compare(const core* lhs, const core* rhs)
{
	return std::lexicographical_compare(
		reinterpret_cast<const T*>(lhs->beg),
		reinterpret_cast<const T*>(lhs->mid),
		reinterpret_cast<const T*>(rhs->beg),
		reinterpret_cast<const T*>(rhs->mid));
}

template<typename T, size_t LocalCapacity>
struct storage
{
	static_assert(alignof(core) >= alignof(T),
		"padding between core and local storage");

	core c;
	std::aligned_storage_t<
		sizeof(T) * LocalCapacity,
		alignof(T)
	> mutable s;
};

template<typename T>
struct storage<T, 0>
{
	core c;
};

template<typename T, typename Allocator>
struct allocator_ebo : Allocator, T
{
	allocator_ebo() = default;

	nobloat_INLINE allocator_ebo(const Allocator& allocator)
		: Allocator(allocator)
	{
	}
};

template<typename T, typename Allocator, size_t LocalCapacity>
class vector
{
	static constexpr bool has_local_storage = LocalCapacity > 0;

#define nobloat_LOCAL_STORAGE_SIZE (LocalCapacity * sizeof(T))

	typedef typename std::allocator_traits<Allocator>
		::template rebind_alloc<byte> byte_allocator;

	typedef std::allocator_traits<byte_allocator> byte_allocator_traits;

#define nobloat_CONSTRUCT_TYPE \
	std::conditional_t<std::is_trivially_constructible_v<T>, byte, T>

#define nobloat_DESTROY_TYPE \
	std::conditional_t<std::is_trivially_destructible_v<T>, byte, T>

#define nobloat_RELOCATE_TYPE \
	std::conditional_t<is_trivially_relocatable_v<T>, byte, T>

#define nobloat_COPY_TYPE \
	std::conditional_t<std::is_trivially_copyable_v<T>, byte, T>

#define nobloat_OPERATIONS \
	detail::operations<nobloat_RELOCATE_TYPE, byte_allocator, has_local_storage>

public:
	typedef T value_type;
	typedef Allocator allocator_type;
	typedef size_t size_type;
	typedef ptrdiff_t difference_type;
	typedef T& reference;
	typedef const T& const_reference;
	typedef T* pointer;
	typedef const T* const_pointer;
	typedef T* iterator;
	typedef const T* const_iterator;
	typedef std::reverse_iterator<T*> reverse_iterator;
	typedef std::reverse_iterator<const T*> const_reverse_iterator;

	nobloat_INLINE vector()
	{
		construct<has_local_storage>(&m.c, { nobloat_LOCAL_STORAGE_SIZE });
	}
	
	explicit nobloat_INLINE vector(const Allocator& allocator)
		: m(byte_allocator(allocator))
	{
		construct<has_local_storage>(&m.c, { nobloat_LOCAL_STORAGE_SIZE });
	}

	explicit nobloat_INLINE vector(size_t count,
		const Allocator& allocator = Allocator())
		: m(byte_allocator(allocator))
	{
		construct<has_local_storage>(&m.c, { nobloat_LOCAL_STORAGE_SIZE });
		resize(count);
	}

	nobloat_INLINE vector(size_t count,
		const T& value, const Allocator& allocator = Allocator())
		: m(byte_allocator(allocator))
	{
		construct<has_local_storage>(&m.c, { nobloat_LOCAL_STORAGE_SIZE });

		resize(count, value);
	}

	nobloat_INLINE vector(const T* first, const T* last,
		const Allocator& allocator = Allocator())
		: m(byte_allocator(allocator))
	{
		construct<has_local_storage>(&m.c, { nobloat_LOCAL_STORAGE_SIZE });

		nobloat_OPERATIONS::template push_back_range<nobloat_COPY_TYPE>(&m.c,
			reinterpret_cast<const nobloat_COPY_TYPE*>(first),
			reinterpret_cast<const nobloat_COPY_TYPE*>(last),
			static_cast<byte_allocator&>(m));
	}

	template<class InputIt, std::enable_if_t<is_iterator_v<InputIt>, int> = 0>
	nobloat_INLINE vector(InputIt first, InputIt last,
		const Allocator& allocator = Allocator())
		: m(byte_allocator(allocator))
	{
		construct<has_local_storage>(&m.c, { nobloat_LOCAL_STORAGE_SIZE });

		nobloat_OPERATIONS::template push_back_range<T, InputIt>(
			&m.c, first, last, static_cast<byte_allocator&>(m));
	}

	nobloat_INLINE vector(std::initializer_list<T> list,
		const Allocator& allocator = Allocator())
		: m(byte_allocator(allocator))
	{
		construct<has_local_storage>(&m.c, { nobloat_LOCAL_STORAGE_SIZE });

		nobloat_OPERATIONS::template push_back_range<nobloat_COPY_TYPE>(&m.c,
			reinterpret_cast<const nobloat_COPY_TYPE*>(list.begin()),
			reinterpret_cast<const nobloat_COPY_TYPE*>(list.end()),
			static_cast<byte_allocator&>(m));
	}

	nobloat_INLINE vector(const vector& other)
		: m(byte_allocator_traits::select_on_container_copy_construction(
				static_cast<const byte_allocator&>(other.m)))
	{
		construct<has_local_storage>(&m.c, { nobloat_LOCAL_STORAGE_SIZE });

		nobloat_OPERATIONS::template push_back_range<nobloat_COPY_TYPE>(&m.c,
			reinterpret_cast<const nobloat_COPY_TYPE*>(other.m.c.beg),
			reinterpret_cast<const nobloat_COPY_TYPE*>(other.m.c.mid),
			static_cast<byte_allocator&>(m));
	}

	nobloat_INLINE vector(const vector& other, const Allocator& allocator)
		: m(byte_allocator(allocator))
	{
		construct<has_local_storage>(&m.c, { nobloat_LOCAL_STORAGE_SIZE });

		nobloat_OPERATIONS::template push_back_range<nobloat_COPY_TYPE>(&m.c,
			reinterpret_cast<const nobloat_COPY_TYPE*>(other.m.c.beg),
			reinterpret_cast<const nobloat_COPY_TYPE*>(other.m.c.mid),
			static_cast<byte_allocator&>(m));
	}

	nobloat_INLINE vector(vector&& other)
		: m(static_cast<byte_allocator&&>(other.m))
	{
		nobloat_OPERATIONS::template move_construct<has_local_storage>(
			&m.c, &other.m.c, static_cast<byte_allocator&>(m),
			{ nobloat_LOCAL_STORAGE_SIZE }, { nobloat_LOCAL_STORAGE_SIZE });
	}

	nobloat_INLINE vector(vector&& other, const Allocator& allocator)
		: m(byte_allocator(allocator))
	{
		if constexpr (byte_allocator_traits::is_always_equal::value)
		{
			nobloat_OPERATIONS::template move_construct<has_local_storage>(
				&m.c, &other.m.c, static_cast<byte_allocator&>(m),
				{ nobloat_LOCAL_STORAGE_SIZE }, { nobloat_LOCAL_STORAGE_SIZE });
		}
		else if (static_cast<const byte_allocator&>(m) ==
			static_cast<const byte_allocator&>(other.m))
		{
			nobloat_OPERATIONS::template move_construct<has_local_storage>(
				&m.c, &other.m.c, static_cast<byte_allocator&>(m),
				{ nobloat_LOCAL_STORAGE_SIZE }, { nobloat_LOCAL_STORAGE_SIZE });
		}
		else
		{
			construct<has_local_storage>(&m.c, nobloat_LOCAL_STORAGE_SIZE);

			nobloat_OPERATIONS::move_elements(&m.c,
				&other.m.c, static_cast<byte_allocator&>(m));
		}
	}

	nobloat_INLINE ~vector()
	{
		destroy<nobloat_DESTROY_TYPE, byte_allocator, has_local_storage>(
			&m.c, static_cast<byte_allocator&>(m));
	}

	nobloat_INLINE vector& operator=(const vector& other)
	{
		if constexpr (!byte_allocator_traits
			::propagate_on_container_copy_assignment::value ||
			byte_allocator_traits::is_always_equal::value)
		{
			nobloat_OPERATIONS::template copy_assign<
				nobloat_DESTROY_TYPE, nobloat_COPY_TYPE>(
				&m.c, &other.m.c, static_cast<byte_allocator&>(m));
		}
		else if (static_cast<const byte_allocator&>(m) ==
			static_cast<const byte_allocator&>(other.m))
		{
			nobloat_OPERATIONS::template copy_assign<
				nobloat_DESTROY_TYPE, nobloat_COPY_TYPE>(
				&m.c, &other.m.c, static_cast<byte_allocator&>(m));
		}
		else
		{
			nobloat_OPERATIONS::template copy_assign_allocator<
				nobloat_DESTROY_TYPE, nobloat_COPY_TYPE>(
				&m.c, &other.m.c, static_cast<byte_allocator&>(m),
				static_cast<const byte_allocator&>(other.m),
				{ nobloat_LOCAL_STORAGE_SIZE });
		}

		return *this;
	}

	nobloat_INLINE vector& operator=(vector&& other)
	{
		if constexpr (byte_allocator_traits::is_always_equal::value)
		{
			nobloat_OPERATIONS::template move_assign<
				nobloat_DESTROY_TYPE, has_local_storage>(
				&m.c, &other.m.c, static_cast<byte_allocator&>(m),
				{ nobloat_LOCAL_STORAGE_SIZE }, { nobloat_LOCAL_STORAGE_SIZE });
		}
		else if (static_cast<const byte_allocator&>(m) ==
			static_cast<const byte_allocator&>(other.m))
		{
			nobloat_OPERATIONS::template move_assign<
				nobloat_DESTROY_TYPE, has_local_storage>(
				&m.c, &other.m.c, static_cast<byte_allocator&>(m));
		}
		else if constexpr (byte_allocator_traits
			::propagate_on_container_move_assignment::value)
		{
			nobloat_OPERATIONS::template
				move_assign_allocator<nobloat_DESTROY_TYPE>(
				&m.c, &other.m.c, static_cast<byte_allocator&>(m),
				static_cast<byte_allocator&>(other.m),
				{ nobloat_LOCAL_STORAGE_SIZE });
		}
		else
		{
			nobloat_OPERATIONS::template
				move_assign_elements<nobloat_DESTROY_TYPE>(
				&m.c, &other.m.c, static_cast<byte_allocator&>(m));
		}

		return *this;
	}

	nobloat_INLINE vector& operator=(std::initializer_list<T> list)
	{
		clear();

		nobloat_OPERATIONS::template push_back_range<nobloat_COPY_TYPE>(&m.c,
			reinterpret_cast<const nobloat_COPY_TYPE*>(list.begin()),
			reinterpret_cast<const nobloat_COPY_TYPE*>(list.end()),
			static_cast<byte_allocator&>(m));

		return *this;
	}

	nobloat_INLINE void assign(size_t count, const T& value)
	{
		clear();

		resize(count, value);
	}

	template<typename InputIt>
	nobloat_INLINE void assign(InputIt first, InputIt last)
	{
		clear();

		nobloat_OPERATIONS::template push_back_range<T, InputIt>(
			&m.c, first, last, static_cast<byte_allocator&>(m));
	}

	nobloat_INLINE void assign(std::initializer_list<T> list)
	{
		clear();

		nobloat_OPERATIONS::template push_back_range<nobloat_COPY_TYPE>(&m.c,
			reinterpret_cast<const nobloat_COPY_TYPE*>(list.begin()),
			reinterpret_cast<const nobloat_COPY_TYPE*>(list.end()),
			static_cast<byte_allocator&>(m));
	}

	nobloat_INLINE Allocator get_allocator() const
	{
		return Allocator(static_cast<const byte_allocator&>(m));
	}

	nobloat_INLINE T& operator[](size_t index)
	{
		assert(index < size());
		return reinterpret_cast<T*>(m.c.beg)[index];
	}

	nobloat_INLINE const T& operator[](size_t index) const
	{
		assert(index < size());
		return reinterpret_cast<const T*>(m.c.beg)[index];
	}

	nobloat_INLINE T& at(size_t index)
	{
		assert(index < size());
		return reinterpret_cast<T*>(m.c.beg)[index];
	}

	nobloat_INLINE const T& at(size_t index) const
	{
		assert(index < size());
		return reinterpret_cast<const T*>(m.c.beg)[index];
	}

	nobloat_INLINE T& front()
	{
		assert(!empty());
		return *reinterpret_cast<T*>(m.c.beg);
	}

	nobloat_INLINE const T& front() const
	{
		assert(!empty());
		return *reinterpret_cast<const T*>(m.c.beg);
	}

	nobloat_INLINE T& back()
	{
		assert(!empty());
		return reinterpret_cast<T*>(m.c.mid)[-1];
	}

	nobloat_INLINE const T& back() const
	{
		assert(!empty());
		return reinterpret_cast<const T*>(m.c.mid)[-1];
	}

	nobloat_INLINE T* data()
	{
		return reinterpret_cast<T*>(m.c.beg);
	}

	nobloat_INLINE const T* data() const
	{
		return reinterpret_cast<const T*>(m.c.beg);
	}

	nobloat_INLINE T* begin()
	{
		return reinterpret_cast<T*>(m.c.beg);
	}

	nobloat_INLINE const T* begin() const
	{
		return reinterpret_cast<const T*>(m.c.beg);
	}

	nobloat_INLINE const T* cbegin() const
	{
		return reinterpret_cast<const T*>(m.c.beg);
	}

	nobloat_INLINE T* end()
	{
		return reinterpret_cast<T*>(m.c.mid);
	}

	nobloat_INLINE const T* end() const
	{
		return reinterpret_cast<const T*>(m.c.mid);
	}

	nobloat_INLINE const T* cend() const
	{
		return reinterpret_cast<const T*>(m.c.mid);
	}

	nobloat_INLINE std::reverse_iterator<T*> rbegin()
	{
		return std::make_reverse_iterator(end());
	}

	nobloat_INLINE std::reverse_iterator<const T*> rbegin() const
	{
		return std::make_reverse_iterator(end());
	}

	nobloat_INLINE std::reverse_iterator<const T*> crbegin() const
	{
		return std::make_reverse_iterator(cend());
	}

	nobloat_INLINE std::reverse_iterator<T*> rend()
	{
		return std::make_reverse_iterator(begin());
	}

	nobloat_INLINE std::reverse_iterator<const T*> rend() const
	{
		return std::make_reverse_iterator(begin());
	}

	nobloat_INLINE std::reverse_iterator<const T*> crend() const
	{
		return std::make_reverse_iterator(cbegin());
	}

	nobloat_INLINE bool empty() const
	{
		return m.c.beg == m.c.mid;
	}

	nobloat_INLINE size_t size() const
	{
		return reinterpret_cast<const T*>(m.c.mid) -
			reinterpret_cast<const T*>(m.c.beg);
	}

	nobloat_INLINE size_t max_size() const
	{
		return std::numeric_limits<size_t>::max();
	}

	nobloat_INLINE void reserve(size_t min_capacity)
	{
		nobloat_OPERATIONS::reserve(&m.c,
			min_capacity * sizeof(T), static_cast<byte_allocator&>(m));
	}

	nobloat_INLINE size_t capacity() const
	{
		return reinterpret_cast<const T*>(m.c.end) -
			reinterpret_cast<const T*>(m.c.beg);
	}

	nobloat_INLINE void shrink_to_fit()
	{
		nobloat_OPERATIONS::shrink_to_fit(&m.c,
			static_cast<byte_allocator&>(m), { nobloat_LOCAL_STORAGE_SIZE });
	}

	nobloat_INLINE void clear()
	{
		detail::clear<nobloat_DESTROY_TYPE>(&m.c);
	}

	nobloat_INLINE T* insert(const T* pos, const T& value)
	{
		assert(
			pos >= reinterpret_cast<const T*>(m.c.beg) &&
			pos <= reinterpret_cast<const T*>(m.c.mid));

		byte* slot = nobloat_OPERATIONS::insert(&m.c,
			reinterpret_cast<byte*>(const_cast<T*>(pos)),
			sizeof(T), static_cast<byte_allocator&>(m));

		return new(slot) T(value);
	}

	nobloat_INLINE T* insert(const T* pos, T&& value)
	{
		assert(
			pos >= reinterpret_cast<const T*>(m.c.beg) &&
			pos <= reinterpret_cast<const T*>(m.c.mid));

		byte* slot = nobloat_OPERATIONS::insert(&m.c,
			reinterpret_cast<byte*>(const_cast<T*>(pos)),
			sizeof(T), static_cast<byte_allocator&>(m));

		return new(slot) T(std::move(value));
	}

	nobloat_INLINE T* insert(const T* pos, size_t count, const T& value)
	{
		assert(
			pos >= reinterpret_cast<const T*>(m.c.beg) &&
			pos <= reinterpret_cast<const T*>(m.c.mid));

		size_t size = count * sizeof(T);

		byte* slots = nobloat_OPERATIONS::insert(&m.c,
			reinterpret_cast<byte*>(const_cast<T*>(pos)),
			size, static_cast<byte_allocator&>(m));

		std::uninitialized_fill(
			reinterpret_cast<T*>(slots),
			reinterpret_cast<T*>(slots + size), value);

		return reinterpret_cast<T*>(slots);
	}

	nobloat_INLINE T* insert(const T* pos, const T* first, const T* last)
	{
		assert(
			pos >= reinterpret_cast<const T*>(m.c.beg) &&
			pos <= reinterpret_cast<const T*>(m.c.mid));

		byte* slots = nobloat_OPERATIONS
			::template insert_range<nobloat_COPY_TYPE>(
				&m.c, reinterpret_cast<byte*>(const_cast<T*>(pos)),
				reinterpret_cast<const nobloat_COPY_TYPE*>(first),
				reinterpret_cast<const nobloat_COPY_TYPE*>(last),
				static_cast<byte_allocator&>(m));

		return reinterpret_cast<T*>(slots);
	}

	template<typename InputIt>
	nobloat_INLINE T* insert(const T* pos, InputIt first, InputIt last)
	{
		assert(
			pos >= reinterpret_cast<const T*>(m.c.beg) &&
			pos <= reinterpret_cast<const T*>(m.c.mid));

		byte* slots = nobloat_OPERATIONS::template insert_range<T, InputIt>(
			&m.c, reinterpret_cast<byte*>(const_cast<T*>(pos)),
			first, last, static_cast<byte_allocator&>(m));

		return reinterpret_cast<T*>(slots);
	}

	nobloat_INLINE T* insert(const T* pos, std::initializer_list<T> list)
	{
		return insert(pos, list.begin(), list.end());
	}

	template<typename... Args>
	nobloat_INLINE T* emplace(const T* pos, Args&&... args)
	{
		assert(
			pos >= reinterpret_cast<const T*>(m.c.beg) &&
			pos <= reinterpret_cast<const T*>(m.c.mid));

		byte* slot = nobloat_OPERATIONS::insert(&m.c,
			reinterpret_cast<byte*>(const_cast<T*>(pos)),
			sizeof(T), static_cast<byte_allocator&>(m));

		return new(slot) T(std::forward<Args>(args)...);
	}

	nobloat_INLINE T* erase(const T* pos)
	{
		assert(
			pos >= reinterpret_cast<const T*>(m.c.beg) &&
			pos <= reinterpret_cast<const T*>(m.c.mid));

		byte* next = detail::erase<nobloat_RELOCATE_TYPE, nobloat_DESTROY_TYPE>(
			&m.c, reinterpret_cast<byte*>(const_cast<T*>(pos)),
			reinterpret_cast<byte*>(const_cast<T*>(pos) + 1));

		return reinterpret_cast<T*>(next);
	}

	nobloat_INLINE T* erase(const T* first, const T* last)
	{
		assert(first <= last &&
			first >= reinterpret_cast<const T*>(m.c.beg) &&
			last <= reinterpret_cast<const T*>(m.c.mid));

		byte* next = detail::erase<nobloat_RELOCATE_TYPE, nobloat_DESTROY_TYPE>(
			&m.c, reinterpret_cast<byte*>(const_cast<T*>(first)),
			reinterpret_cast<byte*>(const_cast<T*>(last)));

		return reinterpret_cast<T*>(next);
	}

	nobloat_INLINE void push_back(const T& value)
	{
		byte* slot = nobloat_OPERATIONS::push_back(&m.c,
			sizeof(T), static_cast<byte_allocator&>(m));

		new(slot) T(value);
	}

	nobloat_INLINE void push_back(T&& value)
	{
		byte* slot = nobloat_OPERATIONS::push_back(&m.c,
			sizeof(T), static_cast<byte_allocator&>(m));

		new(slot) T(std::move(value));
	}

	template<typename... Args>
	nobloat_INLINE T& emplace_back(Args&&... args)
	{
		byte* slot = nobloat_OPERATIONS::push_back(&m.c,
			sizeof(T), static_cast<byte_allocator&>(m));

		return *new(slot) T(std::forward<Args>(args)...);
	}

	nobloat_INLINE void pop_back()
	{
		detail::pop_back<nobloat_DESTROY_TYPE>(&m.c, sizeof(T));
	}

	nobloat_INLINE void resize(size_t count)
	{
		nobloat_OPERATIONS::template resize<
			true, nobloat_CONSTRUCT_TYPE, nobloat_DESTROY_TYPE>(
			&m.c, count * sizeof(T), static_cast<byte_allocator&>(m));
	}

	void resize(size_t count, const T& value)
	{
		byte* pos = nobloat_OPERATIONS::template resize<
			false, void, nobloat_DESTROY_TYPE>(&m.c,
			count * sizeof(T), static_cast<byte_allocator&>(m));

		std::uninitialized_fill(reinterpret_cast<T*>(pos),
			reinterpret_cast<T*>(m.c.mid), value);
	}

	nobloat_INLINE void swap(vector& other)
	{
		if constexpr (!byte_allocator_traits::is_always_equal::value)
		{
			using namespace std;
			swap(static_cast<byte_allocator&>(m),
				static_cast<byte_allocator&>(other.m));
		}

		if constexpr (has_local_storage)
		{
			nobloat_OPERATIONS::swap(&m.c, &other.m.c,
				static_cast<byte_allocator&>(m),
				static_cast<byte_allocator&>(other.m));
		}
		else
		{
			std::swap(m.c, other.m.c);
		}
	}


	allocator_ebo<storage<T, LocalCapacity>, byte_allocator> m;


#undef nobloat_LOCAL_STORAGE_SIZE
#undef nobloat_CONSTRUCT_TYPE
#undef nobloat_DESTROY_TYPE
#undef nobloat_RELOCATE_TYPE
#undef nobloat_COPY_TYPE
#undef nobloat_OPERATIONS
};

template<typename T, typename Allocator, size_t LocalCapacity>
nobloat_INLINE void swap(
	vector<T, Allocator, LocalCapacity>& lhs,
	vector<T, Allocator, LocalCapacity>& rhs)
{
	lhs.swap(rhs);
}

template<typename T, typename Allocator, size_t LocalCapacity>
nobloat_INLINE bool operator==(
	const vector<T, Allocator, LocalCapacity>& lhs,
	const vector<T, Allocator, LocalCapacity>& rhs)
{
	return equal<T>(&lhs.m.c, &rhs.m.c);
}

template<typename T, typename Allocator, size_t LocalCapacity>
nobloat_INLINE bool operator!=(
	const vector<T, Allocator, LocalCapacity>& lhs,
	const vector<T, Allocator, LocalCapacity>& rhs)
{
	return !equal<T>(&lhs.m.c, &rhs.m.c);
}

template<typename T, typename Allocator, size_t LocalCapacity>
nobloat_INLINE bool operator<(
	const vector<T, Allocator, LocalCapacity>& lhs,
	const vector<T, Allocator, LocalCapacity>& rhs)
{
	return compare<T>(&lhs.m.c, &rhs.m.c);
}

template<typename T, typename Allocator, size_t LocalCapacity>
nobloat_INLINE bool operator>(
	const vector<T, Allocator, LocalCapacity>& lhs,
	const vector<T, Allocator, LocalCapacity>& rhs)
{
	return compare<T>(&rhs.m.c, &lhs.m.c);
}

template<typename T, typename Allocator, size_t LocalCapacity>
nobloat_INLINE bool operator<=(
	const vector<T, Allocator, LocalCapacity>& lhs,
	const vector<T, Allocator, LocalCapacity>& rhs)
{
	return !compare<T>(&rhs.m.c, &lhs.m.c);
}

template<typename T, typename Allocator, size_t LocalCapacity>
nobloat_INLINE bool operator>=(
	const vector<T, Allocator, LocalCapacity>& lhs,
	const vector<T, Allocator, LocalCapacity>& rhs)
{
	return !compare<T>(&lhs.m.c, &rhs.m.c);
}

} // namespace detail

template<typename T, typename Allocator = std::allocator<T>>
using vector = detail::vector<T, Allocator, 0>;

template<typename T, size_t LocalCapacity,
	typename Allocator = std::allocator<T>>
using small_vector = detail::vector<T, Allocator, LocalCapacity>;

} // namespace nobloat
